class Symbol implements Comparable<Symbol> {
    String s;

    public Symbol(String s) {
        this.s = s;
    }

    public static Symbol makeSymbol(String s) {
        return new Symbol(s);
    }

    @Override
    public String toString() {
        return this.s;
    }

    @Override
    public boolean equals(Object obj) {
        return (obj instanceof Symbol) && (s.equals(((Symbol)obj).s));
    }

    @Override
    public int hashCode() {
        return s.hashCode();
    }

    @Override
    public int compareTo(Symbol sym) {
        return s.compareTo(sym.s);
    }
}
