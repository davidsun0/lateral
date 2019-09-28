class Helper {

    public static ConsCell makeList(Object ... vals) {
        ConsCell list = null;
        for(int i = 0; i < vals.length; i ++) {
            list = new ConsCell(vals[i], list);
        }
        return list;
    }

    public static void main(String[] args) {
        ConsCell list = makeList("hi", null, Integer.valueOf(123));
        System.out.println(Lateral.length(list, 0));
    }
}
