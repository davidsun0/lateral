import java.util.Scanner;
import java.util.HashMap;
import java.util.Map;
import java.util.ArrayList;
import java.util.Collections;
import java.util.NoSuchElementException;

import java.io.IOException;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.InvocationTargetException;

public class Runtime {
    static Environment userEnvir;

    static {
        HashMap<Object, Object> userTable = new HashMap<>(256);
        userEnvir = new Environment(userTable);

        //*
        for (Method m : Lateral.class.getMethods()) {
            int mod = m.getModifiers();
            if(Modifier.isStatic(mod) && Modifier.isPublic(mod)) {
                userTable.put(new Symbol(toLispName(m.getName())), m);
            }
        }
        //*/

        for (Method m : Lang.class.getMethods()) {
            int mod = m.getModifiers();
            if(Modifier.isStatic(mod) && Modifier.isPublic(mod)) {
                userTable.put(new Symbol(toLispName(m.getName())), m);
            }
        }

        userTable.put(new Symbol("nil"), null);
        userTable.put(new Symbol("t"), Boolean.TRUE);

        try {
            userTable.put(new Symbol("read-atom0"),
                    Helper.class.getMethod("readAtom", Object.class));

            userTable.put(new Symbol("+"), Lang.class.getMethod("add",
                        Object[].class));
            userTable.put(new Symbol("-"), Lang.class.getMethod("subtract",
                        Object[].class));
            userTable.put(new Symbol("//"), Lang.class.getMethod("divide",
                        Object.class, Object.class));
            userTable.put(new Symbol("<"), Lang.class.getMethod("less_than",
                        Object[].class));
            userTable.put(new Symbol("="), Lang.class.getMethod("equal_p",
                        Object.class, Object.class));
            userTable.put(new Symbol("first"), Lang.class.getMethod("car",
                        Object.class));
            userTable.put(new Symbol("rest"), Lang.class.getMethod("cdr",
                        Object.class));

            userTable.put(new Symbol("load-class"), Runtime.class.getMethod(
                        "load_class", Object.class));
            userTable.put(new Symbol("user-envir"), Runtime.class.getMethod(
                        "getUserEnvir"));
            userTable.put(new Symbol("java-name"), Runtime.class.getMethod(
                        "java_name", Object.class));
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        }
    }

    private static String toLispName(String name) {
        String lispName = name;
        if("_p".equals(name.substring(name.length() - 2))) {
            name = name.substring(0, name.length() - 2) + "?";
        } else if("_b".equals(name.substring(name.length() - 2))) {
            name = name.substring(0, name.length() - 2) + "!";
        }
        return name.replace('_', '-');
    }

    public static Object java_name(Object methodName) {
        if(methodName instanceof String) {
            String mname = ((String)methodName)
                    .replace("\\", "\\5C")
                    .replace("<", "\\3C")
                    .replace(">", "\\3E");
            return mname;
        } else {
            throw new TypeError();
        }
    }

    protected static void insertMethod(Object classx, Object javaName,
            Object lispName, Object argc, Object isRest, Object isMacro) {
        if(classx instanceof Class &&
                javaName instanceof String &&
                lispName instanceof String &&
                argc instanceof Integer) {
            boolean rest = isRest == null ? false : true;
            boolean macro = isMacro == null ? false : true;
            try {
                Class<?>[] args = new Class<?>[(Integer)argc];
                for(int i = 0; i < (Integer)argc; i ++) {
                    args[i] = Object.class;
                }
                Method method = ((Class<?>)classx).getMethod((String)javaName,
                            args);
                envir_set(new Symbol((String)lispName),
                        new CompiledLambda(method, (Integer)argc, rest, macro));
                // System.out.println(javaName + " " + argc + " " + method);
            } catch (NoSuchMethodException e) {
                e.printStackTrace();
            }
        } else {
            throw new TypeError("Can't insert method");
        }
    }

    public static Object getUserEnvir() {
        return userEnvir;
    }

    public static Object envir_set(Object name, Object val) {
        // behavior of insert_b is to return the hashmap/environment
        // lisp behavior of def is to return the value
        Lang.insert_b(userEnvir, name, val);
        return val;
    }

    public static Object envir_get(Object name) {
        return Lang.car(Lang.get0(userEnvir, name));
    }

    public static Object load_class(Object bl) {
        if(!(bl instanceof ConsCell)) {
            throw new TypeError();
        }

        System.out.println(Lateral.class.getClassLoader());
        System.out.println(Lang.class.getClassLoader());

        ConsCell byteList = (ConsCell)bl;
        int length = (Integer)Lateral.length(byteList);

        byte[] bytes = new byte[length];
        int i = 0;

        while(byteList != null) {
            if(byteList.getCar() instanceof Integer) {
                bytes[i] = ((Integer)byteList.getCar()).byteValue();
                i++;
            } else {
                throw new TypeError();
            }
            byteList = byteList.getCdr();
        }

        Class<?> c = new FunctionLoader().defineClass(bytes);

        ConsCell output = new ConsCell(null, null);
        ConsCell curr = output;

        for (Method m : c.getMethods()) {
            int mod = m.getModifiers();
            if(Modifier.isStatic(mod) && Modifier.isPublic(mod)) {
                Lang.insert_b(userEnvir, new Symbol(toLispName(m.getName())), m);
                ConsCell item = new ConsCell(m.getName(),
                        new ConsCell(m, null));
                curr.setCdr(new ConsCell(null, null));
                curr = curr.getCdr();
                curr.setCar(item);
            }
        }
       return output.getCdr();
    }

    /*
    public static Object eval(Object expr) {
        return Lateral.apply(expr, userEnvir);
    }
    */

    public static void main(String[] args) {
        while(true) {
            try {
                Lateral.main();
            } catch(NoSuchElementException e) {
                System.out.println();
                return;
            } catch (RuntimeException e) {
                e.printStackTrace();
            }
        }
    }
}
