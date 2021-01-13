class TypeError extends RuntimeException {
    TypeError() {
        this("Type Error");
    }

    TypeError(String message) {
        super(message);
    }
}
