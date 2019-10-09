public class Lambda {
    ConsCell args;
    Object expr;
    boolean isMacro;

    public Lambda(ConsCell args, Object expr, boolean isMacro) {
        this.args = args;
        this.expr = expr;
        this.isMacro = isMacro;
    }

    public String toString() {
        if(isMacro)
            return "<macro>";
        else
            return "<lambda>";
    }
}
