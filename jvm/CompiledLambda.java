import java.lang.reflect.Method;

public class CompiledLambda {
    Method method;
    boolean isRest;
    boolean isMacro;
    int argc;

    public CompiledLambda(Method method, int argc, boolean isRest, boolean isMacro) {
        this.method = method;
        this.argc = argc;
        this.isRest = isRest;
        this.isMacro = isMacro;
    }

    public String toString() {
        return "<compiled-lambda>";
    }
}
