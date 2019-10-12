import java.util.NoSuchElementException;

class Helper {
    public static void main(String[] args) {
        Lang.include("core2.lisp");
        while(true) {
            try {
                if(args.length > 0)
                    LateralB.main();
                else
                    Lateral.main();
            } catch (NoSuchElementException n) {
                return;
            } catch (RuntimeException e) {
                e.printStackTrace();
            }
        }
    }

    // These functions should be written in lisp, but I'm too lazy
    public static Object readAtom(Object a) {
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
            return new Keyword(s);
        } else if(s.length() > 2 && s.charAt(0) == '0' && s.charAt(1) == 'x') {
            return Integer.parseInt(s.substring(2), 16);
        } else if(48 <= s.charAt(0) && s.charAt(0) < 58) {
            return Integer.parseInt(s);
        } else {
            return new Symbol(s);
        }
    }

    public static Object isList(Object a) {
        if(a instanceof ConsCell) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }

    public static Object isSymbol(Object a) {
        if(a instanceof Symbol) {
            return Boolean.TRUE;
        } else {
            return null;
        }
    }
}
