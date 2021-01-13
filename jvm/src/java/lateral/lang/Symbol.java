package lateral.lang;

public final class Symbol {
    private final String value;
    private final int hash;

    private Symbol(String value){
        this.value = value;
        hash = value.hashCode();
    }

    public static Symbol makeSymbol(String value) {
        // TODO: Symbol interning
        return new Symbol(value);
    }

    static private int gensymCount = -1;

    public static Symbol gensym(String prefix) {
        gensymCount ++;
        // TODO: check if symbol exists already
        return Symbol.makeSymbol(prefix + gensymCount);
    }

    public int hashCode() {
        return hash;
    }

    public boolean equals(Object obj) {
        if(obj == this)
            return true;
            return obj instanceof Symbol
                    && hash == ((Symbol) obj).hash
                    && value.equals(((Symbol) obj).value);
    }

    public String toString() {
        return value;
    }
}
