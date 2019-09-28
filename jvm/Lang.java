import java.util.Scanner;

class Lang {
    static Scanner scanner;
    static {
        scanner = new Scanner(System.in);
    }

    public static String readLine() {
        return scanner.nextLine();
    }

    public static Object inc(Object o) {
        if(o instanceof Integer) {
            return Integer.valueOf(((Integer)o).intValue() + 1);
        } else {
            return null;
        }
    }

    public static Object cdr(Object o) {
        if(o instanceof ConsCell) {
            return ((ConsCell)o).getCdr();
        } else {
            return null;
        }
    }

    public static Object cons(Object c, Object l) {
        if(l instanceof ConsCell) {
            return new ConsCell(c, (ConsCell)l);
        } else {
            return null;
        }
    }

    public static void main(String[] args) {
        System.out.println(Boolean.TRUE);
        System.out.println(Lateral.not(null));
        System.out.println(inc("test"));
        System.out.println(Lateral.length(null, 10));
    }
}
