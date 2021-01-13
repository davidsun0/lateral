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

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        ConsCell cell = this;
        while(cell != null) {
            sb.append(cell.car);
            if(cell.cdr != null) {
                sb.append(" ");
            }
            cell = cell.cdr;
        }
        sb.append(")");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if(o instanceof ConsCell) {
            ConsCell c = (ConsCell)o;
            return (car.equals(c.car) &&
                    ((cdr == null && c.cdr == null) ||
                     (cdr.equals(c.cdr))));
        }
        return false;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        if(car != null) {
            hash += car.hashCode();
        }
        if(cdr != null) {
            hash += cdr.hashCode();
        }
        return hash;
    }
}
