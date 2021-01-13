package lateral.lang;

import java.util.ArrayList;

/**
 * Parent class of all Lateral functions.
 * TODO: convert to interface?
 */

abstract public class Function {
    abstract public boolean isMacro();
    public String toString() {
        return "#<function>";
    }

    /**
     * apply is the preferred run-time way to call a first class function object.
     * apply should dispatch to calls of Function.invoke(). invoke methods should be
     * public virtual and be non-void. invoke is the preferred compile-time way to
     * call an object. If the function object represents a varargs call, the last
     * argument of invoke should be of type Sequence.
     *
     * As an optimization, invokedynamic calls resolved by Environment will attempt to
     * directly call invoke with the given arguments instead of apply.
     *
     * @param args The objects of the function call
     * @return The result of calling this function on the given arguments
     * @throws UnsupportedOperationException
     * When the function cannot be applied to the given number of arguments
     */
    abstract public Object apply(Object ... args);

    public static Object apply(Function function, Sequence arglist) {
        ArrayList<Object> args = new ArrayList<>();
        for(; !arglist.isEmpty(); arglist = arglist.rest()) {
            args.add(arglist.first());
        }
        return function.apply(args.toArray());
    }
}
