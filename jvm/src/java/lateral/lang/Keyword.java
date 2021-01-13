package lateral.lang;

public final class Keyword {
    private final String value;
    private final int hash;

    private Keyword(String value) {
        this.value = value;
        hash = value.hashCode();
    }

    public static Keyword makeKeyword(String value) {
        // TODO: Keyword interning
        return new Keyword(value);
    }

    public int hashCode() {
        return hash;
    }

    public boolean equals(Object obj) {
        if(obj == this) {
            return true;
        } else {
            return obj instanceof Keyword
                    && hash == ((Keyword) obj).hash
                    && value.equals(((Keyword) obj).value);
        }
    }

    public String getValue() {
        return value;
    }

    public String toString() {
        return ":" + value;
    }
}
