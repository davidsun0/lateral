package lateral.lang;

public class SyntaxException extends RuntimeException {
    public SyntaxException() {
        super();
    }

    public SyntaxException(String message) {
        super(message);
    }

    public SyntaxException(Throwable cause) {
        super(cause);
    }
}
