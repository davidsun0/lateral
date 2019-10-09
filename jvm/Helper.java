class Helper {
    public static void main(String[] args) {
        while(true) {
            try {
                Lateral.main();
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
        if("nil".equals(s)) {
            return null;
        } else if("t".equals(s)) {
            return Boolean.TRUE;
        } else if(s.charAt(0) == '"' && s.charAt(s.length() - 1) == '"') {
            return s.substring(1, s.length() - 1);
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
