package lateral.lang;

/**
 * Simple linked list implementation of Sequence
 */

public class LinkedList extends Sequence {
    private Object value;
    private Sequence next;

    public LinkedList(Object value) {
        this(value, EmptySequence.EMPTY_SEQUENCE);
    }

    public LinkedList(Object value, Sequence next){
        this.value = value;
        if(next == null)
            throw new RuntimeException("Sequence cannot be null");
        this.next = next;
    }

    public Object first() {
        return value;
    }

    public Sequence rest() {
        return next;
    }

    public Object nth(int n) {
        Sequence sequence = this;
        for(int i = 0; i < n; i ++) {
            sequence = sequence.rest();
        }
        return sequence.first();
    }

    public int size() {
        Sequence sequence = this;
        int count = 0;
        while(!sequence.isEmpty()) {
            sequence = sequence.rest();
            count ++;
        }
        return count;
    }
}
