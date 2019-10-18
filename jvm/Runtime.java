import java.util.Scanner;
import java.util.HashMap;
import java.util.Map;
import java.util.ArrayList;
import java.util.Collections;

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
    static FunctionLoader classLoader;

    static {
        classLoader = new FunctionLoader();

        HashMap<Object, Object> userTable = new HashMap<>(256);

        for (Method m : Lang.class.getMethods()) {
            int mod = m.getModifiers();
            if(Modifier.isStatic(mod) && Modifier.isPublic(mod)) {
                //System.out.println(m.getName());
                //System.out.println(toLispName(m.getName()));
                userTable.put(new Symbol(toLispName(m.getName())), m);
            }
        }

        userTable.put(new Symbol("nil"), null);
        userTable.put(new Symbol("t"), Boolean.TRUE);

        try {
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

            userTable.put(new Symbol("list"), Runtime.class.getMethod("list",
                        Object[].class));
            userTable.put(new Symbol("eval"), Runtime.class.getMethod("eval",
                        Object.class));
            userTable.put(new Symbol("load-class"), Runtime.class.getMethod(
                        "load_class", Object.class));
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        }
        userEnvir = new Environment(userTable);
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


    public static Object getUserEnvir() {
        return userEnvir;
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

        Class c = classLoader.defineClass(bytes);

        for (Method m : c.getMethods()) {
            int mod = m.getModifiers();
            if(Modifier.isStatic(mod) && Modifier.isPublic(mod)) {
                Lang.insert_b(userEnvir, new Symbol(toLispName(m.getName())), m);
            }
        }
       return null;
    }

    public static Object eval(Object expr) {
        return Lateral.apply(expr, userEnvir);
    }

    public static Object list(Object ... args) {
        ConsCell output = new ConsCell(null, null);
        ConsCell end = output;
        for(int i = 0; i < args.length; i ++) {
            end.setCdr(new ConsCell(args[i], null));
            end = end.getCdr();
        }
        return output.getCdr();
    }
}
