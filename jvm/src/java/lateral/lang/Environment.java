package lateral.lang;

import java.lang.invoke.*;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;

public class Environment {

    private static HashMap<Symbol, ValueCall> calls = new HashMap<>();

    public static Object insert(Symbol name, Object value) {
        if (calls.containsKey(name)) {
            Environment.ValueCall valueCall = calls.get(name);
            valueCall.update(value, name.toString());
        } else {
            calls.put(name, new Environment.ValueCall(value));
        }
        return value;
    }

    public static Object get(Symbol symbol) {
        Object ret = getIfExists(symbol);
        if(ret == null)
            throw new RuntimeException("Can't find symbol in environment: " + symbol);
        return ret;
    }

    public static Object getIfExists(Symbol symbol) {
        ValueCall valueCall = calls.get(symbol);
        if (valueCall != null) {
            return valueCall.value;
        } else {
            return null;
        }
    }

    /**
     * The Environmental dynamic object lookup method.
     * For use with invokedynamic instructions expecting a value or a function of no arguments
     * and produces an object value.
     *
     * @param lookup Not used. All functions are found with MethodHandles.publicLookup().
     * @param dynamicName The name of the object in the environment.
     * @param dynamicType A method type which should take no parameters and returns an Object.
     * @param namespace Reserved for future use.
     * @return A CallSite of no arguments which produces the object of the dynamicName
     *
     * The CallSite returned may throw an exception if the function request can't be fulfilled.
     * If the needed resource is fulfilled in the future, the returned CallSite will be mutated
     * with the new value and no longer throw an exception.
     *
     */
    public static CallSite dynamicObject(MethodHandles.Lookup lookup,
                                         String dynamicName,
                                         MethodType dynamicType,
                                         String namespace) {
        // TODO: move dynamicName to extra arg to prevent illegal naming
        /*
        dynamicName is stored in extra arguments because second arg must be valid unqualified name:
        https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.2.2
        none of .;[/ and no <> unless the method name is <init> or <clinit>
        If the second arg is going to be namespace information, namespaces will also have to follow these
        rules. Maybe just leave it unused?
        */
        ValueCall valueCall;
        Symbol symbol = Symbol.makeSymbol(dynamicName);
        if (calls.containsKey(symbol)) {
            valueCall = calls.get(symbol);
        } else {
            // generate
            valueCall = new ValueCall();
            calls.put(symbol, valueCall);
        }
        return valueCall.requestObject(dynamicName);
    }

    /**
     * Finds a function by name and returns its CallSite.
     * For use with invokedynamic instructions expecting a function call.
     *
     * @param lookup      The lookup handle from the invokedynamic instruction.
     * @param unused      Unused. Must be a valid JVM unqualified name.
     * @param dynamicName The name of the function to be called.
     * @param dynamicType The expected method type of the function.
     * @return A CallSite representing the function invocation.
     *
     * The CallSite returned may throw an exception if the function request can't be fulfilled.
     * If the needed resource is fulfilled in the future, the returned CallSite will be mutated
     * with the new value and no longer throw an exception.
     */
    public static CallSite dynamicFunction(MethodHandles.Lookup lookup,
                                           String unused,
                                           MethodType dynamicType, String dynamicName) {
        /*
        dynamicName is stored in extra arguments because second arg must be valid unqualified name:
        https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.2.2
        none of .;[/ and no <> unless the method name is <init> or <clinit>
        If the second arg is going to be namespace information, namespaces will also have to follow these
        rules. Maybe just leave it unused?
        */
        Environment.ValueCall valueCall;
        Symbol symbol = Symbol.makeSymbol(dynamicName);
        if (calls.containsKey(symbol)) {
            valueCall = calls.get(symbol);
        } else {
            valueCall = new Environment.ValueCall();
            calls.put(symbol, valueCall);
        }
        return valueCall.requestFunction(dynamicType, dynamicName);
    }

    static MethodHandle errorHandle(String message, MethodType methodType) {
        return MethodHandles.dropArguments(
                MethodHandles.throwException(Object.class, RuntimeException.class)
                        .bindTo(new RuntimeException(message)),
                0, methodType.parameterList());
    }

    static class ValueCall {

        static MethodType CONSTANT_TYPE = MethodType.methodType(Object.class);

        Object value;
        boolean exists;
        MutableCallSite asObject;
        ArrayList<MutableCallSite> asFunction = new ArrayList<>();

        ValueCall() {
            exists = false;
        }

        ValueCall(Object value) {
            exists = true;
            this.value = value;
        }

        static MethodHandle objectHandle(ValueCall valueCall, String name) {
            if (!valueCall.exists) {
                return errorHandle(String.format("Couldn't find object %s", name), CONSTANT_TYPE);
            } else {
                return MethodHandles.constant(Object.class, valueCall.value).asType(CONSTANT_TYPE);
            }
        }

        static MethodHandle functionHandle(ValueCall valueCall, MethodType methodType, String name) {
            if (!valueCall.exists) {
                return errorHandle(String.format("Couldn't find function %s", name), CONSTANT_TYPE);
            } else if (!(valueCall.value instanceof Function)) {
                return errorHandle(String.format("Object %s can't be used as a function", name), CONSTANT_TYPE);
            }
            Function function = (Function) valueCall.value;
            if (function.isMacro()) {
                return errorHandle(String.format("Macro %s can't be used as a function", name), CONSTANT_TYPE);
            }
            try {
                Method invoke = function.getClass().getMethod("invoke", methodType.parameterArray());
                return MethodHandles.publicLookup().unreflect(invoke).bindTo(function);
            } catch (NoSuchMethodException e) {
                try {
                    for (Method m : function.getClass().getMethods()) {
                        if ("invoke".equals(m.getName())) {
                            Class<?>[] params = m.getParameterTypes();
                            if (params[params.length - 1] == Sequence.class && params.length - 1 <= methodType.parameterCount()) {
                                MethodHandle base = MethodHandles.publicLookup().unreflect(m).bindTo(function);
                                // asCollector collects (given - expected + 1) arguments into an Array
                                // the array is fed into ArraySequence.makeList, which returns the varargs as a single Sequence
                                MethodHandle makelist = MethodHandles.publicLookup().findStatic(ArraySequence.class, "makeList",
                                        MethodType.methodType(Sequence.class, Object[].class)
                                ).asCollector(Object[].class, methodType.parameterCount() - m.getParameterCount() + 1);
                                // all but the first expected - 1 arguments are fed into the makelist described above
                                // This effectively combines the two MethodHandles into one with automatic varargs to Sequence collection
                                return MethodHandles.collectArguments(base, m.getParameterCount() - 1, makelist);
                            }
                        }
                    }
                } catch (NoSuchMethodException | IllegalAccessException e2) {
                    e2.printStackTrace();
                }
            } catch (IllegalAccessException i) {
                i.printStackTrace();
            }
            return errorHandle(String.format("Function %s cannot be called as %s", name, methodType.toString()),
                    CONSTANT_TYPE);
        }

        void update(Object value, String name) {
            exists = true;
            this.value = value;
            ArrayList<MutableCallSite> mutableCallSites = new ArrayList<>(asFunction);
            // TODO: update all calls
            if (asObject != null) {
                asObject.setTarget(objectHandle(this, name));
                mutableCallSites.add(asObject);
            }
            for (MutableCallSite callSite : asFunction) {
                callSite.setTarget(functionHandle(this, callSite.getTarget().type(), name));
            }
            if (!mutableCallSites.isEmpty())
                MutableCallSite.syncAll(mutableCallSites.toArray(new MutableCallSite[0]));
        }

        MutableCallSite requestObject(String name) {
            if (this.asObject == null) {
                this.asObject = new MutableCallSite(objectHandle(this, name));
            }
            return this.asObject;
        }

        MutableCallSite requestFunction(MethodType methodType, String name) {
            for (MutableCallSite site : asFunction) {
                if (site.type().equals(methodType)) {
                    return site;
                }
            }
            MutableCallSite site = new MutableCallSite(functionHandle(this, methodType, name));
            asFunction.add(site);
            return site;
        }
    }
}
