package lateral.lang;

/**
 * Sequence backed by an array for better performance
 */
public class ArraySequence extends Sequence {

    private Object[] values;
    private int index;
    private Sequence next;

    public ArraySequence(Object ... values) {
        this(values, 0);
    }

    public ArraySequence(Object[] values, int index) {
        this(values, index, EmptySequence.EMPTY_SEQUENCE);
    }

    public ArraySequence(Object[] values, int index, Sequence next) {
        this.values = values;
        if(index < 0 || index >= values.length)
            throw new RuntimeException("invalid ArraySequence");
        this.index = index;
        this.next = next;
    }

    public Object first() {
        return values[index];
    }

    public Sequence rest() {
        if(index + 1 >= values.length) {
            return next;
        } else {
            return new ArraySequence(values, index + 1, next);
        }
    }

    public Object nth(int n) {
        if(index + n < values.length) {
            return values[index + n];
        } else {
            return next.nth(n - (values.length - index));
        }
    }

    public int size() {
        int myLength = values.length - index;
        return myLength + next.size();
    }
}
