class ConsCell {
    private Object car;
    private ConsCell cdr;

    public ConsCell(Object car, ConsCell cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    public Object getCar() {
        return car;
    }

    public void setCar(Object car) {
        this.car = car;
    }

    public ConsCell getCdr() {
        return cdr;
    }

    public void setCdr(ConsCell cdr) {
        this.cdr = cdr;
    }
}
