package lateral.lang;

import org.objectweb.asm.Type;

import java.lang.invoke.MethodType;
import java.util.ArrayList;
import java.util.Collections;

/**
 * Compilation unit representing a JVM Class
 */
class CompClass {
    static int CLASS_NUM = 0;

    String name;
    Symbol functionName;
    ArrayList<Object> members;
    private ArrayList<Symbol> captured;
    boolean isMacro = false;

    // arities is the list containing argument counts of non-varargs invokes
    ArrayList<Integer> arities = new ArrayList<>();
    /*
    Each function can only have one vararg arity (otherwise it would be ambiguous)
    varargsCount is the number of non-vararg arguments the vararg arity has
    if varargsCount is -1, this function does not have a vararg arity
     */
    int varargsCount = -1;

    CompClass() {
        this(null);
    }

    CompClass(Symbol functionName) {
        members = new ArrayList<>();
        captured = new ArrayList<>();
        this.name = "AnonFunc" + (CLASS_NUM ++);
        this.functionName = functionName;
    }

    String getClassName() {
        return this.name;
    }

    Symbol getFunctionName() {
        return functionName;
    }

    String getConstructor() {
        return Assembler.getMethodDescriptor(void.class, captured.size());
    }

    void addCaptured(Symbol symbol) {
        for(Symbol capture : captured) {
            if(symbol.equals(capture))
                return;
        }
        captured.add(symbol);
    }

    ArrayList<Symbol> getCaptured() {
        return captured;
    }

    void generateConstructor() {
        ArrayList<Object> opcodes = new ArrayList<>();
        opcodes.add(Sequence.makeList(Assembler.ALOAD, 0));
        opcodes.add(Sequence.makeList(
                Assembler.INVOKESPECIAL,
                Type.getInternalName(Function.class),
                "<init>",
                "()V"));

        opcodes.add(Sequence.makeList(Assembler.ALOAD, 0));
        int localSlotNum = 1;
        for(Symbol sym : captured) {
            // generate byecode to set fields
            opcodes.add(Assembler.DUP);
            opcodes.add(Sequence.makeList(Assembler.ALOAD, localSlotNum));
            opcodes.add(Sequence.makeList(
                    Assembler.PUTFIELD,
                    name, sym.toString(), Type.getDescriptor(Object.class)
            ));
            // also generate fields
            members.add(Sequence.makeList(Assembler.DEFFIELD, sym.toString(), Type.getDescriptor(Object.class)));
            localSlotNum ++;
        }
        opcodes.add(Assembler.RETURN);

        // bundle into method
        Sequence header = Sequence.makeList(
                Assembler.DEFMETHOD, "<init>",
                getConstructor(), EmptySequence.EMPTY_SEQUENCE
        );
        Sequence body = Sequence.makeList(opcodes.toArray());
        members.add(Sequence.concat(new ArraySequence(header, body)));
    }

    void generateInvoker(int paramCount, boolean isVarargs, ArrayList<Object> opcodes) {
        if(isVarargs) {
            if(varargsCount == -1)
                varargsCount = paramCount - 1;
            else
                throw new RuntimeException();
        } else {
            // TODO: check arity doesn't already exist
            arities.add(paramCount);
        }

        Class<?>[] params = Assembler.getParameterClasses(paramCount);
        if(isVarargs)
            params[params.length - 1] = Sequence.class;

        /*
        TODO: use java.util.function interfaces
        or make my own
        In compiler, avoid expensive (?) calls through varargs apply
        0: Supplier                     get()
        1: Function/UnaryOperator       apply(A)
        2: BiFunction/BinaryOperator    apply(A, B)
         */
        Sequence header = Sequence.makeList(
                Assembler.DEFMETHOD, "invoke",
                MethodType.methodType(Object.class, params).toMethodDescriptorString(),
                EmptySequence.EMPTY_SEQUENCE);

        Sequence body = new ArraySequence(opcodes.toArray());
        members.add(Sequence.concat(new ArraySequence(header, body)));
    }

    void addInvokerCase(ArrayList<Object> opcodes, int argc) {
        // load function itself
        opcodes.add(Sequence.makeList(Assembler.ALOAD, 0));
        for(int i = 0; i < argc; i ++) {
            // load args[i] onto stack
            opcodes.add(Sequence.makeList(Assembler.ALOAD, 1));
            opcodes.add(Sequence.makeList(Assembler.ICONST, i));
            opcodes.add(Assembler.AALOAD);
        }
        // call appropriate invoke
        String descriptor = Assembler.getMethodDescriptor(Object.class, argc);
        opcodes.add(Sequence.makeList(Assembler.INVOKEVIRTUAL, this.name, "invoke", descriptor));
        opcodes.add(Assembler.ARETURN);
    }

    void addVarargsInvokerCase(ArrayList<Object> opcodes, Symbol errLabel) {
        // only one varargs arity
        opcodes.add(Sequence.makeList(Assembler.ICONST, varargsCount));
        opcodes.add(Sequence.makeList(Assembler.IF_ICMPLT, errLabel));
        opcodes.add(Sequence.makeList(Assembler.ALOAD, 0));
        for(int i = 0; i < varargsCount; i ++) {
            // load args[i] onto stack
            opcodes.add(Sequence.makeList(Assembler.ALOAD, 1));
            opcodes.add(Sequence.makeList(Assembler.ICONST, i));
            opcodes.add(Assembler.AALOAD);
        }
        opcodes.add(Sequence.makeList(Assembler.ALOAD, 1));
        opcodes.add(Sequence.makeList(Assembler.ICONST, varargsCount));
        opcodes.add(Sequence.makeList(Assembler.INVOKESTATIC,
                Type.getInternalName(Sequence.class), "makeList",
                MethodType.methodType(Sequence.class, Object[].class, int.class).toMethodDescriptorString()));

        // call appropriate invoke
        Class<?>[] paramTypes = Assembler.getParameterClasses(varargsCount + 1);
        paramTypes[varargsCount] = Sequence.class;
        String descriptor = MethodType.methodType(Object.class, paramTypes).toMethodDescriptorString();
        // String descriptor = Assembler.getMethodDescriptor(Object.class, argc);
        opcodes.add(Sequence.makeList(Assembler.INVOKEVIRTUAL, this.name, "invoke", descriptor));
        opcodes.add(Assembler.ARETURN);
    }

    void generateInherits(boolean isMacro) {
        this.isMacro = isMacro;
        members.add(Sequence.makeList(
                Assembler.DEFMETHOD, "isMacro", "()Z", EmptySequence.EMPTY_SEQUENCE,
                Sequence.makeList(Assembler.ICONST, isMacro ? 1 : 0),
                Assembler.IRETURN));

        // APPLY GENERATOR IS NOT FUN TO WRITE IN JAVA
        /*
        Object apply(Object ... args) {
            switch(args.length) {
                case 0:
                    return this.invoke();
                case 1:
                    return this.invoke(args[0]);
                ...
                default:
                    if(args.length > varargsCount)
                        return this.invoke(args[0] ... args[varargsCount -1],
                            new ArraySequence(args, varargsCount));
                    else
                        throw new RuntimeError();
            }
        }
         */
        ArrayList<Object> applyOps = new ArrayList<>();
        // header
        applyOps.add(Assembler.DEFMETHOD);
        applyOps.add("apply");
        applyOps.add("([Ljava/lang/Object;)Ljava/lang/Object;");
        applyOps.add(EmptySequence.EMPTY_SEQUENCE);

        // body
        applyOps.add(Sequence.makeList(Assembler.ALOAD, 1));
        applyOps.add(Assembler.ARRAYLENGTH);
        Symbol errLabel = Symbol.gensym("error");

        // TODO: optimize for seamless varargs (no holes, every apply call is valid) to not throw error
        if(arities.size() == 1) {
            // simple if statement for single arity functions
            applyOps.add(Sequence.makeList(Assembler.ICONST, arities.get(0)));
            applyOps.add(Sequence.makeList(Assembler.IF_ICMPNE, errLabel));
            addInvokerCase(applyOps, arities.get(0));
            // single arity cannot be seamless
        } else if(arities.size() == 0 && varargsCount != -1) {
            addVarargsInvokerCase(applyOps, errLabel);
            // varargs is seamless if varargsCount == 0
        } else if(arities.size() > 1) {
            // sort arities
            Collections.sort(arities);
            // seamless if arities start at 0 and varargsCount is equal to smallest hole
            Symbol[] arityLabels = new Symbol[arities.size()];
            for (int i = 0; i < arities.size(); i++) {
                arityLabels[i] = Symbol.gensym("table");
            }
            Symbol defaultLabel = Symbol.gensym("default");
            applyOps.add(Sequence.makeList(Assembler.LOOKUPSWITCH,
                    defaultLabel, new ArraySequence(arities.toArray()),
                    new ArraySequence((Object[]) arityLabels)));

            for (int i = 0; i < arities.size(); i++) {
                // lookupswitch label
                applyOps.add(Sequence.makeList(Assembler.LABEL, arityLabels[i]));
                addInvokerCase(applyOps, arities.get(i));
            }
            applyOps.add(Sequence.makeList(Assembler.LABEL, defaultLabel));
            if(varargsCount != -1)
                addVarargsInvokerCase(applyOps, errLabel);
        } else {
            throw new RuntimeException();
        }
        applyOps.add(Sequence.makeList(Assembler.LABEL, errLabel));
        applyOps.add(Sequence.makeList(Assembler.NEW, Type.getInternalName(RuntimeException.class)));
        applyOps.add(Assembler.DUP);
        /*
        // display debug info about the array
        applyOps.add(ArraySequence.makeList(Assembler.ALOAD, 1));
        applyOps.add(ArraySequence.makeList(Assembler.INVOKESTATIC, Type.getInternalName(Arrays.class),
                "deepToString", "([Ljava/lang/Object;)Ljava/lang/String;"));
        applyOps.add(ArraySequence.makeList(Assembler.INVOKESPECIAL,
                Type.getInternalName(RuntimeException.class),
                "<init>", "(Ljava/lang/String;)V"));
         */
        applyOps.add(Sequence.makeList(Assembler.INVOKESPECIAL,
                Type.getInternalName(RuntimeException.class),
                "<init>", "()V"));
        applyOps.add(Assembler.ATHROW);
        members.add(Sequence.makeList(applyOps.toArray()));
    }

    Sequence toTree() {
        Sequence header = Sequence.makeList(
                Assembler.DEFCLASS,
                getClassName(),
                // meta?
                EmptySequence.EMPTY_SEQUENCE);
        Sequence body = new ArraySequence(members.toArray());
        // TODO: better concat
        return Sequence.concat(new ArraySequence(header, body));
    }

    void generateToString(String name) {
        String value = String.format("#<%s %s>", isMacro ? "macro" : "function", name);
        members.add(Sequence.makeList(
                Assembler.DEFMETHOD, "toString", "()Ljava/lang/String;", EmptySequence.EMPTY_SEQUENCE,
                Sequence.makeList(Assembler.LDC, value),
                Assembler.ARETURN
        ));
    }
}
