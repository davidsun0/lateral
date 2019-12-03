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

public class Lang {
    static Scanner scanner;

    static {
        scanner = new Scanner(System.in);
    }

    protected static Object nativeInvoke(Object function, Object args) {
        if(function instanceof Method) {
            Method m = (Method)function;
            ConsCell a = (ConsCell)args;
            Object[] arglist;
            if(m.isVarArgs()) {
                ArrayList<Object> arglist0 = new ArrayList<>();
                while(a != null) {
                    arglist0.add(car(a));
                    a = (ConsCell)cdr(a);
                }
                arglist = arglist0.toArray();
            } else {
                arglist = new Object[m.getParameterCount()];
                for(int i = 0; i < m.getParameterCount(); i ++) {
                    arglist[i] = car(a);
                    a = (ConsCell)cdr(a);
                }
            }
            try{
                if(m.isVarArgs())
                    return m.invoke(null, (Object) arglist);
                else
                    return m.invoke(null, arglist);
            } catch (IllegalAccessException | ExceptionInInitializerError e) {
                System.out.println("failed to invoke method " + m);
                e.printStackTrace();
            } catch (IllegalArgumentException b) {
                System.out.println("invalid arguments for method " + m);
                System.out.println(arglist);
                b.printStackTrace();
            } catch (InvocationTargetException c) {
                System.out.println("error occured while invoking method " + m);
                c.printStackTrace();
            }
            throw new RuntimeException("failed to invoke method " + m);
        } else {
            throw new TypeError(String.format("can't invoke %s as function",
                        function));
        }
    }

    protected static Object readAtom(Object a) {
        if(a == null || !(a instanceof String)) {
            throw new TypeError("readAtom expects non-null string argument");
        }
        String s = (String)a;
        if(s.length() > 1 && s.charAt(0) == '"' && s.charAt(s.length() - 1) == '"') {
            s = s.substring(1, s.length() - 1);
            s = s.replace("\\\\", "\\");
            s = s.replace("\\n", "\n");
            s = s.replace("\\\"", "\"");
            return s;
        } else if(s.charAt(0) == ':') {
            return new Keyword(s.substring(1));
        } else if(s.length() == 3 && "#\\".equals(s.substring(0, 2))) {
            return Character.valueOf(s.charAt(2));
        } else if(s.length() > 2 && s.charAt(0) == '0' && s.charAt(1) == 'x') {
            return Integer.parseInt(s.substring(2), 16);
        } else if(48 <= s.charAt(0) && s.charAt(0) < 58) {
            return Integer.parseInt(s);
        } else {
            return new Symbol(s);
        }
    }

    public static Object write_bytes(Object p, Object b) {
        System.out.println("writing bytes...");
        if(p instanceof String && b instanceof ConsCell) {
            String path = (String)p;
            ConsCell byteList = (ConsCell)b;
            while(byteList != null) {
                if(!(car(byteList) instanceof Integer)) {
                    System.out.format("can't write %s as byte\n",
                            car(byteList));
                    return null;
                }
                byteList = byteList.getCdr();
            }

            byteList = (ConsCell)b;
            try (OutputStream ostream = new FileOutputStream(path)) {
                while(byteList != null) {
                    int writeByte = (Integer)car(byteList);
                    ostream.write(writeByte);
                    byteList = byteList.getCdr();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            return null;
        }
        throw new TypeError();
    }

    public static Object slurp(Object s) {
        if(!(s instanceof String))
            throw new TypeError();

        try{
            String path = (String)s;
            String content = new String(Files.readAllBytes(Paths.get(path)));
            return content;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    /*
    public static Object include(Object s) {
        if(!(s instanceof String))
            throw new TypeError();

        try {
            String path = (String)s;
            String content = new String(Files.readAllBytes(Paths.get(path)));
            Object tokens = Lateral.tokenize(content, 0, 0, null, null);
            Object envir = Runtime.getUserEnvir();
            ConsCell expr;
            while((expr = (ConsCell)Lateral.readForm(tokens)) != null) {
                Lateral.apply(car(expr), envir);
                tokens = Lateral.nth(1, expr);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
    */

    public static Object flatten(Object tree) {
        if(tree == null || !(tree instanceof ConsCell)) {
            return tree;
        }
        Object[] treeStack = new Object[256];
        int stackTop = 0;
        ConsCell output = new ConsCell(null, null);
        ConsCell curr = output;
        while(stackTop != 0 || tree != null) {
            if(tree == null) {
                stackTop --;
                if(stackTop < 0) {
                    System.out.println("flatten stack underflow");
                    break;
                }
                else
                    tree = treeStack[stackTop];
            } else if(car(tree) != null && car(tree) instanceof ConsCell) {
                treeStack[stackTop] = cdr(tree);
                stackTop ++;
                tree = car(tree);
            } else {
                curr.setCdr(new ConsCell(null, null));
                curr = curr.getCdr();
                curr.setCar(car(tree));

                tree = cdr(tree);
            }
        }
        return cdr(output);
    }

    public static Object hashmap() {
        return new HashMap<Object, Object>();
    }

    public static Object keyvals(Object map) {
        if(!(map instanceof HashMap)) {
            throw new TypeError("keyvals");
        }

        HashMap<Object, Object> h = (HashMap<Object, Object>)map;
        ConsCell outList = new ConsCell(null, null);
        ConsCell curr = outList;
        for(Map.Entry<Object, Object> entry : h.entrySet()) {
            ConsCell c = new ConsCell(entry.getKey(),
                    new ConsCell(entry.getValue(), null));
            curr.setCdr(new ConsCell(null, null));
            curr = curr.getCdr();
            curr.setCar(c);
        }
        return cdr(outList);
    }

    public static Object symbol(Object a) {
        if(a instanceof String) {
            return new Symbol((String)a);
        } else if(a instanceof Symbol) {
            return a;
        } else {
            throw new TypeError("symbol");
        }
    }

    public static Object get_args(Object l) {
        if(l instanceof Lambda) {
            return ((Lambda)l).args;
        } else {
            throw new TypeError();
        }
    }

    public static Object get_expr(Object l) {
        if(l instanceof Lambda) {
            return ((Lambda)l).expr;
        } else {
            throw new TypeError();
        }
    }

    protected static Object lambda(Object params, Object expr) {
        if(params == null || params instanceof ConsCell) {
            return new Lambda((ConsCell)params, expr, false);
        } else {
            throw new TypeError();
        }
    }

    protected static Object lambda(Object params, Object expr,
            Environment envir) {
        if(params == null || params instanceof ConsCell) {
            return new Lambda((ConsCell)params, expr, false);
        } else {
            throw new TypeError();
        }
    }

    // lambda?
    public static Object lambda_p(Object l) {
        if(l instanceof Lambda && !((Lambda)l).isMacro) {
            return Boolean.TRUE;
        //} else if(l instanceof NativeFunction) {
        } else if(l instanceof Method) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    // native?
    public static Object native_p(Object fn) {
        if(fn instanceof Method) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    protected static Object macro(Object a, Object e) {
        if(a == null || a instanceof ConsCell) {
            return new Lambda((ConsCell)a, e, true);
        } else {
            throw new TypeError();
        }
    }

    // macro?
    public static Object macro_p(Object l) {
        if(l instanceof Lambda && ((Lambda)l).isMacro) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    public static Object list_p(Object a) {
        if(a instanceof ConsCell) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    public static Object symbol_p(Object a) {
        if(a instanceof Symbol) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    public static Object int_p(Object a) {
        return a instanceof Integer ? Boolean.TRUE : null;
    }

    public static Object string_p(Object a) {
        return a instanceof String ? Boolean.TRUE : null;
    }

    public static Object keyword_p(Object a) {
        return a instanceof Keyword ? Boolean.TRUE : null;
    }

    // type
    public static Object type(Object o) {
        if(o == null || o == Boolean.TRUE) {
            return o;
        } else if(o instanceof Symbol) {
            return new Keyword("symbol");
        } else if(o instanceof Integer) {
            return new Keyword("int");
        } else if(o instanceof String) {
            return new Keyword("string");
        } else if(o instanceof Character) {
            return new Keyword("char");
        } else if(o instanceof Keyword) {
            return new Keyword("keyword");
        } else if(o instanceof ConsCell) {
            return new Keyword("list");
        } else if(o instanceof HashMap) {
            return new Keyword("hashmap");
        } else if(o instanceof Lambda) {
            if(((Lambda)o).isMacro) {
                return new Keyword("macro");
            } else {
                return new Keyword("function");
            }
        } else {
            System.out.println(o.getClass());
            throw new RuntimeException("unknown object type");
        }
    }

    public static Object contains_p(Object h, Object k) {
        if(h instanceof HashMap) {
            HashMap map = (HashMap)h;
            if(map.containsKey(k)) {
                return Boolean.TRUE;
            } else {
                return null;
            }
        } else if(h instanceof Environment) {
            Environment e = (Environment)h;
            if(e.contains(k)) {
                return Boolean.TRUE;
            } else {
                return null;
            }
        } else {
            System.out.println(h);
            System.out.println(h.getClass());
            throw new TypeError();
        }
    }

    public static Object get0(Object h, Object k) {
        if(h instanceof HashMap) {
            HashMap map = (HashMap)h;
            if(map.containsKey(k)) {
                return new ConsCell(map.get(k), new ConsCell(Boolean.TRUE, null));
            } else {
                return null;
            }
        } else if(h instanceof Environment) {
            Environment e = (Environment)h;
            return e.get(k);
        } else {
            throw new TypeError();
        }
    }

    // remove when not needed by lateral
    public static Object get(Object h, Object k) {
        return get0(h, k);
    }

    public static Object insert_b(Object h, Object k, Object v) {
        if(h instanceof HashMap) {
            HashMap map = (HashMap)h;
            map.put(k, v);
            return map;
        } else if(h instanceof Environment) {
            HashMap map = ((Environment)h).map;
            map.put(k, v);
            return map;
        } else {
            throw new TypeError();
        }
    }

    public static Object insert(Object h, Object k, Object v) {
        if(h instanceof HashMap) {
            HashMap map = (HashMap)h;
            HashMap newMap = (HashMap)map.clone();
            newMap.put(k, v);
            return newMap;
        } else {
            throw new TypeError();
        }
    }

    public static Object make_envir(Object e) {
        if(e == null || e instanceof Environment) {
            Environment envir = new Environment((Environment)e);
            return envir;
        } else {
            throw new TypeError();
        }
    }

    /*
     * CHAR AND STRING FUNCTIONS
     */
    public static Object keyword(Object o) {
        String s;
        if(o instanceof Keyword) {
            return o;
        } else if(o instanceof String) {
            s = (String)o;
        } else if(o instanceof Symbol) {
            s = ((Symbol)o).toString();
        } else {
            throw new TypeError("keyword");
        }
        return new Keyword(s);
    }

    public static Object string(Object ... args) {
        StringBuilder sb = new StringBuilder();
        for(int i = 0; i < args.length; i ++) {
            Object o = args[i];
            sb.append(o);
        }
        return sb.toString();
    }

    public static Object string0(Object args) {
        if(args instanceof ConsCell) {
            ConsCell arglist = (ConsCell)args;
            StringBuilder sb = new StringBuilder();
            while(arglist != null) {
                sb.append(arglist.getCar());
                arglist = arglist.getCdr();
            }
            return sb.toString();
        } else {
            throw new TypeError();
        }
    }

    public static Object readLine() {
        return scanner.nextLine() + "\n";
    }

    public static Object whitespace_p(Object c) {
        if(c instanceof Character) {
            if (Character.isWhitespace((Character)c)) {
                return Boolean.TRUE;
            } else {
                return null;
            }
        } else {
            throw new TypeError();
        }
    }

    public static Object char_at(Object s, Object i) {
        if(s instanceof String && i instanceof Integer) {
            String str = (String)s;
            int idx = ((Integer)i).intValue();
            if(idx < 0 || idx >= str.length())
                return null;
            else
                return Character.valueOf(str.charAt(idx));
        } else {
            throw new TypeError();
        }
    }

    public static Object to_char(Object s) {
        if(s instanceof Integer) {
            int x = ((Integer)s).intValue();
            return Character.valueOf((char)x);
        } else if(s instanceof String) {
            return Character.valueOf(((String)s).charAt(0));
        } else {
            throw new TypeError();
        }
    }

    public static Object substr(Object s, Object x, Object y) {
        if(s instanceof String && x instanceof Integer && y instanceof Integer) {
            return ((String)s).substring((Integer)x, (Integer)y);
        } else {
            throw new TypeError();
        }
    }

    /*
     * PRINT FUNCTIONS
     */

    public static Object print0(Object o) {
        if(o == null) {
            System.out.print("nil");
        } else if(o instanceof ConsCell) {
            ConsCell c = (ConsCell) o;
            System.out.print("(");
            while(c != null) {
                print0(c.getCar());
                if(c.getCdr() != null)
                    System.out.print(" ");
                c = c.getCdr();
            }
            System.out.print(")");
        } else if(o instanceof Character) {
            System.out.printf("'%c'", ((Character)o).charValue());
        } else if(o instanceof String) {
            System.out.print("\"" + o + "\"");
        } else if(o instanceof Keyword) {
            System.out.print(":" + o);
        } else if(o instanceof Method) {
            System.out.print("<native-fn>");
        } else {
            System.out.print(o.toString());
        }

        return null;
    }

    public static Object pprint0(Object o) {
        if(o == null) {
            System.out.print("nil");
        } else if(o instanceof ConsCell) {
            ConsCell c = (ConsCell) o;
            System.out.print("(");
            while(c != null) {
                pprint0(c.getCar());
                if(c.getCdr() != null)
                    System.out.print(" ");
                c = c.getCdr();
            }
            System.out.print(")");
        } else if(o instanceof Keyword) {
            System.out.print(":" + o);
        } else if(o instanceof Method) {
            System.out.print("<native-fn>");
        } else {
            System.out.print(o.toString());
        }
        return null;
    }

    /*
     * NUMERICAL FUNCTIONS
     */
    public static Object less_than(Object ... args) {
        if(!(args[0] instanceof Integer)) {
            System.out.println(args[0]);
            throw new TypeError("first arg of < is not int");
        }

        int lastVal = (Integer)args[0];

        for(int i = 1; i < args.length; i ++) {
            Object term = args[i];
            if(term instanceof Integer) {
                if(lastVal >= (Integer)term)
                    return null;
                lastVal = (Integer)term;
            } else {
                throw new TypeError("less than expects integers");
            }
        }
        return Boolean.TRUE;
    }

    public static Object less_than0(Object a, Object b) {
        if(!(a instanceof Integer && b instanceof Integer)) {
            throw new TypeError();
        }

        if((Integer)a >= (Integer)b) {
            return null;
        } else {
            return Boolean.TRUE;
        }
    }

    public static Object subtract(Object ... args) {
        int diff = (Integer)args[0];
        if(args.length == 1) {
            return Integer.valueOf(-diff);
        }

        for(int i = 1; i < args.length; i ++) {
            Object term = args[i];
            if(term instanceof Integer) {
                diff -= (Integer)term;
            } else {
                throw new TypeError("subtraction expects integers");
            }
        }
        return Integer.valueOf(diff);
    }

    public static Object add(Object ... args) {
        int sum = 0;
        for(int i = 0; i < args.length; i ++) {
            Object term = args[i];
            if(term instanceof Integer) {
                sum += (Integer)term;
            } else {
                System.out.print(term.getClass());
                throw new TypeError("addition expects integers");
            }
        }
        return Integer.valueOf(sum);
    }

    public static Object add0(Object a, Object b) {
        if(a instanceof Integer && b instanceof Integer) {
            return Integer.valueOf(((Integer)a) + ((Integer)b));
        } else {
            throw new TypeError("add0 " + a + " " + b);
        }
    }

    public static Object subtract0(Object a, Object b) {
        if(a instanceof Integer && b instanceof Integer) {
            return Integer.valueOf(((Integer)a) - ((Integer)b));
        } else {
            throw new TypeError("subtract0 " + a + " " + b);
        }
    }

    public static Object integer(Object o) {
        if(o instanceof Character) {
            return Integer.valueOf(((Character)o).charValue());
        } else if(o instanceof Integer) {
            return o;
        } else if(o instanceof String) {
            return Integer.parseInt((String)o);
        } else {
            throw new TypeError("integer");
        }
    }

    public static Object bit_asr(Object a, Object b) {
        if(a instanceof Integer && b instanceof Integer) {
            int result = ((Integer)a).intValue() >> ((Integer)b).intValue();
            return Integer.valueOf(result);
        } else {
            throw new TypeError("bit-asr");
        }
    }

    public static Object bit_and(Object a, Object b) {
        if(a instanceof Integer && b instanceof Integer) {
            int result = ((Integer)a).intValue() & ((Integer)b).intValue();
            return Integer.valueOf(result);
        } else {
            throw new TypeError("bit-and");
        }
    }

    public static Object divide(Object a, Object b) {
        if(a instanceof Integer && b instanceof Integer) {
            int result = ((Integer)a).intValue() / ((Integer)b).intValue();
            return Integer.valueOf(result);
        } else {
            throw new TypeError("int division");
        }
    }

    //TODO: remove after no longer needed by lateral.lisp
    public static Object inc(Object o) {
        if(o instanceof Integer) {
            return Integer.valueOf(((Integer)o).intValue() + 1);
        } else {
            throw new TypeError("inc expects Integer, but got " + o.getClass());
        }
    }

    //TODO: remove after no longer needed by lateral.lisp
    public static Object dec(Object o) {
        if(o instanceof Integer) {
            return Integer.valueOf(((Integer)o).intValue() - 1);
        } else {
            throw new TypeError("dec expects Integer, but got " + o.getClass());
        }
    }

    public static Object equal_p(Object a, Object b) {
        if(a == b) {
            return Boolean.TRUE;
        } else if(a == null || b == null) {
            return null;
        } else if(a instanceof Character && b instanceof String) {
            if(((String)b).length() == 1 && a.equals(((String)b).charAt(0)))
                return Boolean.TRUE;
            else
                return null;
        } else if(a.equals(b)) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    protected static Object isNumericallyEqual(Object a, Object b) {
        if(a instanceof Integer && b instanceof Integer) {
            if(((Integer)a).intValue() == ((Integer)b).intValue()) {
                return Boolean.TRUE;
            } else {
                return null;
            }
        } else {
            throw new TypeError("= expects two Integers, but got " +
                    a.getClass() + " " + b.getClass());
        }
    }

    /*
     * LIST FUNCTIONS
     */

    public static Object car(Object o) {
        if(o == null) {
            return null;
        } else if(o instanceof ConsCell) {
            return ((ConsCell)o).getCar();
        } else {
            throw new TypeError();
        }
    }

    public static Object cdr(Object o) {
        if(o == null) {
            return null;
        } else if(o instanceof ConsCell) {
            return ((ConsCell)o).getCdr();
        } else {
            throw new TypeError("cdr expects type ConsCell, but got " + o.getClass());
        }
    }

    public static Object cons0(Object c, Object l) {
        if(l == null || l instanceof ConsCell) {
            return new ConsCell(c, (ConsCell)l);
        } else {
            throw new TypeError("cons expects type ConsCell, but got " + l.getClass());
        }
    }

    // to delete when not used by Lateral
    public static Object cons(Object c, Object l) {
        return cons0(c, l);
    }
}
