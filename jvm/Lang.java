import java.util.Scanner;

class Lang {
    static Scanner scanner;
    static {
        scanner = new Scanner(System.in);
    }

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

    public static Object inc(Object o) {
        if(o instanceof Integer) {
            return Integer.valueOf(((Integer)o).intValue() + 1);
        } else {
            throw new TypeError("inc expects Integer, but got " + o.getClass());
        }
    }

    public static Object isEqual(Object a, Object b) {
        if(a == b) {
            return Boolean.TRUE;
        } else if(a == null || b == null) {
            return null;
        } else if(a.equals(b)) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    public static Object car(Object o) {
        if(o instanceof ConsCell) {
            return ((ConsCell)o).getCar();
        } else {
            throw new TypeError();
        }
    }

    public static Object cdr(Object o) {
        if(o instanceof ConsCell) {
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
