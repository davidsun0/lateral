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
}
