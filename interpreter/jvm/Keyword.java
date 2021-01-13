class Keyword {
    String s;

    public Keyword(String s) {
        this.s = s;
    }

    public static Keyword makeKeyword(String s) {
        return new Keyword(s);
    }

    @Override
    public String toString() {
        return this.s;
    }

    @Override
    public boolean equals(Object obj) {
        return (obj instanceof Keyword) && (s.equals(((Keyword)obj).s));
    }

    @Override
    public int hashCode() {
        return s.hashCode();
    }
}
