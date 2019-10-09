import java.util.HashMap;

class Environment {
    HashMap<Object, Object> map;
    Environment outer;

    public Environment() {
        map = new HashMap<>();
        outer = null;
    }

    public Environment(HashMap<Object, Object> map) {
        this.map = map;
        outer = null;
    }

    public Environment(Environment e) {
        this();
        this.outer = e;
    }

    public Object get(Object key) {
        if(map.containsKey(key)) {
            return new ConsCell(map.get(key), new ConsCell(Boolean.TRUE, null));
        } else if(outer != null) {
            return outer.get(key);
        } else {
            throw new RuntimeException(
                    String.format("Key %s not found in environment", key));
        }
    }
}
