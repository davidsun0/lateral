import java.util.Scanner;
import java.util.HashMap;
import java.util.Collections;
import java.util.ArrayList;
import java.io.IOException;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;

class Lang {
    static Scanner scanner;
    static Environment userEnvir;

    static {
        scanner = new Scanner(System.in);

        HashMap<Object, Object> userTable = new HashMap<>(256);

        userTable.put(new Symbol("nil"), null);
        userTable.put(new Symbol("t"), Boolean.TRUE);

        userTable.put(new Symbol("write-bytes"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                Object p = car(args);
                Object b = nth(1, args);
                if(p instanceof String && b instanceof ConsCell) {
                    String path = (String)p;
                    ConsCell byteList = (ConsCell)b;
                    try (OutputStream ostream = new FileOutputStream(path)) {
                        while(byteList != null) {
                            if(car(byteList) instanceof Integer) {
                                int writeByte = (Integer)car(byteList);
                                ostream.write(writeByte);
                            } else {
                                System.out.format("can't write %s as byte\n",
                                        car(byteList));
                            }
                            byteList = byteList.getCdr();
                        }
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    return null;
                }
                throw new TypeError();
            }
        });

        userTable.put(new Symbol("flatten"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                Object tree = car(args);
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
        });

        userTable.put(new Symbol("keyvals"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                if(!(car(args) instanceof HashMap)) {
                    throw new TypeError("keyvals");
                }

                HashMap<Object, Object> h = (HashMap<Object, Object>)car(args);
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
        });

        userTable.put(new Symbol("symbol"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                Object a = car(args);
                if(a instanceof String) {
                    return new Symbol((String)a);
                } else if(a instanceof Symbol) {
                    return a;
                } else {
                    throw new TypeError("symbol");
                }
            }
        });

        userTable.put(new Symbol("//"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                Object a = car(args);
                Object b = nth(1, args);
                if(a instanceof Integer && b instanceof Integer) {
                    int result = ((Integer)a).intValue() / ((Integer)b).intValue();
                    return Integer.valueOf(result);
                } else {
                    throw new TypeError("int division");
                }
            }
        });

        userTable.put(new Symbol("bit-and"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                Object a = car(args);
                Object b = nth(1, args);
                if(a instanceof Integer && b instanceof Integer) {
                    int result = ((Integer)a).intValue() & ((Integer)b).intValue();
                    return Integer.valueOf(result);
                } else {
                    throw new TypeError("bit-and");
                }
            }
        });

        userTable.put(new Symbol("bit-asr"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                Object a = car(args);
                Object b = nth(1, args);
                if(a instanceof Integer && b instanceof Integer) {
                    int result = ((Integer)a).intValue() >> ((Integer)b).intValue();
                    return Integer.valueOf(result);
                } else {
                    throw new TypeError("bit-asr");
                }
            }
        });


        userTable.put(new Symbol("eval"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return car(Lateral.eval(args, userEnvir, null));
            }
        });

        userTable.put(new Symbol("integer"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                Object o = car(args);
                if(o instanceof Character) {
                    return Integer.valueOf(((Character)o).charValue());
                } else if(o instanceof Integer) {
                    return o;
                } else {
                    throw new TypeError("integer");
                }
            }
        });

        userTable.put(new Symbol("print-env"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                HashMap map = userEnvir.map;
                ArrayList<Symbol> keys = new ArrayList<>();
                for(Object o : map.keySet()) {
                    keys.add((Symbol)o);
                }
                Collections.sort(keys);
                for(Object s : keys) {
                    System.out.println(s + "\t\t" + map.get(s));
                }
                return null;
            }
        });

        userTable.put(new Symbol("string"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                Object o;
                StringBuilder sb = new StringBuilder();
                while((o = car(args)) != null) {
                    if(o instanceof Keyword) {
                        sb.append(((Keyword)o).toString().substring(1));
                    } else {
                        sb.append(o);
                    }
                    args = (ConsCell)cdr(args);
                }
                return sb.toString();
            }
        });

        userTable.put(new Symbol("keyword"), new NativeFunction () {
            @Override
            public Object invoke(ConsCell args) {
                Object o = car(args);
                String s;
                if(o instanceof Keyword) {
                    return o;
                } else if(o instanceof String) {
                    s = ":" + (String)o;
                } else if(o instanceof Symbol) {
                    s = ((Symbol)o).toString();
                } else {
                    throw new TypeError("keyword");
                }
                return new Keyword(s);
            }
        });
                
        userTable.put(new Symbol("type"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return getType(car(args));
            }
        });

        userTable.put(new Symbol("+"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                Object term;
                int sum = 0;
                while(args != null) {
                    if((term = args.getCar()) instanceof Integer) {
                        sum += (Integer)term;
                    } else {
                        println(car(args));
                        throw new TypeError("addition expects integers");
                    }
                    args = args.getCdr();
                }
                return Integer.valueOf(sum);
            }
        });

        userTable.put(new Symbol("-"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                int diff = (Integer)args.getCar();
                if(args.getCdr() == null) {
                    return Integer.valueOf(-diff);
                }
                args = args.getCdr();

                Object term;
                while(args != null) {
                    if((term = args.getCar()) instanceof Integer) {
                        diff -= (Integer)term;
                    } else {
                        throw new TypeError("addition expects integers");
                    }
                    args = args.getCdr();
                }
                return Integer.valueOf(diff);
            }
        });

        userTable.put(new Symbol("<"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                if(!(car(args) instanceof Integer)) {
                    println(car(args));
                    throw new TypeError("first arg of < is not int");
                }

                int lastVal = (Integer)car(args);
                args = (ConsCell)cdr(args);

                Object term;
                while(args != null) {
                    if((term = args.getCar()) instanceof Integer) {
                        if(lastVal >= (Integer)term)
                            return null;
                        lastVal = (Integer)term;
                    } else {
                        throw new TypeError("less than expects integers");
                    }
                    args = (ConsCell)cdr(args);
                }
                return Boolean.TRUE;
            }
        });

        userTable.put(new Symbol("list"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return args;
            }
        });

        userTable.put(new Symbol("first"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return car(nth(0, args));
            }
        });

        userTable.put(new Symbol("rest"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return cdr(nth(0, args));
            }
        });

        userTable.put(new Symbol("cons"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return cons(nth(0, args), nth(1, args));
            }
        });

        userTable.put(new Symbol("hashmap"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return new HashMap<Object, Object>();
            }
        });

        // get
        userTable.put(new Symbol("get"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return get(nth(0, args), nth(1, args));
            }
        });

        // insert
        userTable.put(new Symbol("insert!"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return insert(nth(0, args), nth(1, args), nth(2, args));
            }
        });

        // print
        userTable.put(new Symbol("print"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                while(args != null) {
                    print(car(args));
                    System.out.print(" ");
                    args = args.getCdr();
                }
                System.out.println();
                return null;
            }
        });

        // pprint
        userTable.put(new Symbol("pprint"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                while(args != null) {
                    pprint(car(args));
                    System.out.print(" ");
                    args = args.getCdr();
                }
                System.out.println();
                return null;
            }
        });

        // equal?
        userTable.put(new Symbol("equal?"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return isEqual(nth(0, args), nth(1, args));
            }
        });

        // =
        userTable.put(new Symbol("="), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return isNumericallyEqual(nth(0, args), nth(1, args));
            }
        });

        userTable.put(new Symbol("include"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return include(nth(0, args));
            }
        });

        userTable.put(new Symbol("char-at"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                return charAt(car(args), nth(1, args));
            }
        });

        userEnvir = new Environment(userTable);
    }

    public static Object nth(int i, ConsCell list) {
        for(int x = 0; x < i; x ++) {
            //println(list);
            //System.out.println(list);
            list = (ConsCell)cdr(list);
        }
        //println(list);
        return car(list);
    }

    public static Object nativeInvoke(Object function, Object args) {
        if(args != null && !(args instanceof ConsCell)) {
            throw new TypeError("args must be a list");
        } else if(function instanceof NativeFunction) {
            return ((NativeFunction)function).invoke((ConsCell)args);
        } else {
            throw new TypeError(String.format("can't invoke %s as function",
                        function));
        }
    }

    public static Object getArgs(Object l) {
        if(l instanceof Lambda) {
            return ((Lambda)l).args;
        } else {
            throw new TypeError();
        }
    }

    public static Object getExpr(Object l) {
        if(l instanceof Lambda) {
            return ((Lambda)l).expr;
        } else {
            throw new TypeError();
        }
    }

    public static Object makeLambda(Object a, Object e) {
        if(a == null || a instanceof ConsCell) {
            return new Lambda((ConsCell)a, e, false);
        } else {
            throw new TypeError();
        }
    }

    public static Object isLambda(Object l) {
        if(l instanceof Lambda && !((Lambda)l).isMacro) {
            return Boolean.TRUE;
        } else if(l instanceof NativeFunction) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    public static Object isNativeFunction(Object fn) {
        if(fn instanceof NativeFunction) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    public static Object makeMacro(Object a, Object e) {
        if(a == null || a instanceof ConsCell) {
            return new Lambda((ConsCell)a, e, true);
        } else {
            throw new TypeError();
        }
    }

    public static Object isMacro(Object l) {
        if(l instanceof Lambda && ((Lambda)l).isMacro) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    // string? symbol? keyword? char? int? list? map?
    public static Object getType(Object o) {
        if(o == null || o == Boolean.TRUE) {
            return o;
        } else if(o instanceof Symbol) {
            return new Keyword(":symbol");
        } else if(o instanceof Integer) {
            return new Keyword(":int");
        } else if(o instanceof String) {
            return new Keyword(":string");
        } else if(o instanceof Character) {
            return new Keyword(":char");
        } else if(o instanceof Keyword) {
            return new Keyword(":keyword");
        } else if(o instanceof ConsCell) {
            return new Keyword(":list");
        } else if(o instanceof HashMap) {
            return new Keyword(":hashmap");
        } else if(o instanceof NativeFunction) {
            return new Keyword(":function");
        } else if(o instanceof Lambda) {
            if(((Lambda)o).isMacro) {
                return new Keyword(":macro");
            } else {
                return new Keyword(":function");
            }
        } else {
            System.out.println(o.getClass());
            throw new RuntimeException("unknown object type");
        }
    }

    public static Object contains(Object h, Object k) {
        if(h instanceof HashMap) {
            HashMap map = (HashMap)h;
            if(map.containsKey(k)) {
                return Boolean.TRUE;
            } else {
                return null;
            }
        } else if(h instanceof Environment) {
            Environment e = (Environment)h;
            if(e.map.containsKey(k)) {
                return Boolean.TRUE;
            } else {
                return null;
            }
        } else {
            throw new TypeError();
        }
    }

    public static Object get(Object h, Object k) {
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

    public static Object insert(Object h, Object k, Object v) {
        if(h instanceof HashMap) {
            HashMap map = (HashMap)h;
            map.put(k, v);
            return map;
        } else if(h instanceof Environment) {
            /*
            print(k);
            System.out.print(" ");
            println(v);
            */
            HashMap map = ((Environment)h).map;
            map.put(k, v);
            if(h == userEnvir) {
            }
            return map;
        } else {
            throw new TypeError();
        }
    }

    public static Object getUserEnvir() {
        return userEnvir;
    }

    public static Object makeEnvir(Object e) {
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

    public static String readLine() {
        return scanner.nextLine() + "\n";
    }

    public static Object include(Object s) {
        if(!(s instanceof String))
            throw new TypeError();

        try {
            String path = (String)s;
            String content = new String(Files.readAllBytes(Paths.get(path)));
            Object tokens = Lateral.tokenize(content, 0, 0, null, null);
            Object envir = getUserEnvir();
            ConsCell expr;
            while((expr = (ConsCell)Lateral.readForm(tokens)) != null) {
                //println(Lateral.apply(car(expr), envir));
                //System.out.println("next form: ");
                //println(car(expr));
                Lateral.apply(car(expr), envir);
                tokens = nth(1, expr);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static Object isWhitespace(Object c) {
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

    public static Object charAt(Object s, Object i) {
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

    public static Object toChar(Object s) {
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

    public static Object print(Object o) {
        if(o == null) {
            System.out.print("nil");
        } else if(o instanceof ConsCell) {
            ConsCell c = (ConsCell) o;
            System.out.print("(");
            while(c != null) {
                print(c.getCar());
                if(c.getCdr() != null)
                    System.out.print(" ");
                c = c.getCdr();
            }
            System.out.print(")");
        } else if(o instanceof Character) {
            System.out.printf("'%c'", ((Character)o).charValue());
        } else if(o instanceof String) {
            System.out.print("\"" + o + "\"");
        } else {
            System.out.print(o.toString());
        }

        return null;
    }

    public static Object pprint(Object o) {
        if(o == null) {
            System.out.print("nil");
        } else if(o instanceof ConsCell) {
            ConsCell c = (ConsCell) o;
            System.out.print("(");
            while(c != null) {
                print(c.getCar());
                if(c.getCdr() != null)
                    System.out.print(" ");
                c = c.getCdr();
            }
            System.out.print(")");
        } else {
            System.out.print(o.toString());
        }
        return null;
    }

    public static Object println(Object o) {
        print(o);
        System.out.println();
        return null;
    }

    /*
     * NUMERICAL FUNCTIONS
     */

    public static Object inc(Object o) {
        if(o instanceof Integer) {
            return Integer.valueOf(((Integer)o).intValue() + 1);
        } else {
            throw new TypeError("inc expects Integer, but got " + o.getClass());
        }
    }

    public static Object dec(Object o) {
        if(o instanceof Integer) {
            return Integer.valueOf(((Integer)o).intValue() - 1);
        } else {
            throw new TypeError("dec expects Integer, but got " + o.getClass());
        }
    }

    public static Object isEqual(Object a, Object b) {
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

    public static Object isNumericallyEqual(Object a, Object b) {
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

    public static ConsCell cons(Object c, Object l) {
        if(l == null || l instanceof ConsCell) {
            return new ConsCell(c, (ConsCell)l);
        } else {
            throw new TypeError("cons expects type ConsCell, but got " + l.getClass());
        }
    }
}
