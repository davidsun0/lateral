import java.util.HashMap;

class Environment {
    HashMap<Object, Object> map;
    private Environment outer;

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

    public HashMap<Object, Object> getMap() {
        return map;
    }

    public Object get(Object key) {
        Environment e = this;
        while(e != null) {
            if(e.map.containsKey(key)) {
                return new ConsCell(e.map.get(key),
                        new ConsCell(Boolean.TRUE, null));
            }
            e = e.outer;
        }
        throw new RuntimeException(
                String.format("Key %s not found in environment", key));
    }

    public boolean contains(Object key) {
        Environment e = this;
        while(e != null) {
            if(e.map.containsKey(key)) {
                return true;
            }
            e = e.outer;
        }
        return false;
    }
}
