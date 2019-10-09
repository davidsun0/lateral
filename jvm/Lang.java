import java.util.Scanner;
import java.util.HashMap;

class Lang {
    static Scanner scanner;
    static Environment userEnvir;

    static {
        scanner = new Scanner(System.in);

        HashMap<Object, Object> userTable = new HashMap<>();
        userTable.put(new Symbol("+"), new NativeFunction() {
            @Override
            public Object invoke(ConsCell args) {
                Object term;
                int sum = 0;
                while(args != null) {
                    if((term = args.getCar()) instanceof Integer) {
                        sum += (Integer)term;
                    } else {
                        throw new TypeError("addition expects integers");
                    }
                    args = args.getCdr();
                }
                return Integer.valueOf(sum);
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
        userEnvir = new Environment(userTable);
    }

    public static Object nth(int i, ConsCell list) {
        for(int x = 0; x < i; i ++) {
            list = list.getCdr();
        }
        return list.getCar();
    }

    public static Object nativeInvoke(Object function, Object args) {
        if(!(args instanceof ConsCell)) {
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
            HashMap map = ((Environment)h).map;
            map.put(k, v);
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
