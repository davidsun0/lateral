package lateral.lang;

import java.lang.invoke.*;

/**
 *     "Back in my day," your grandfather explains, "we didn't have no invokedynamic
 * bootstrap methods! We had to dynamically generate JVM bytecode on the fly and
 * extend ClassLoader.findClass by hand!
 *     "You kids these days have it easy. You only need one bytecode instruction
 * and then even high school script kiddies can write a bootstrap method."
 * A single tear rolls down his wrinkled cheek.
 *     "It's so easy to get access the JIT compiler these days." His voice softens.
 * "There's no more respect for the craft..." His voice trails off. Nobody else
 * is listening anymore.
 *     "There was a time before the JIT compiler where we even had to hand
 * optimize javac's output, but that's not necessary in today's day and age.
 * There's no need for that anymore... There's no need for me anymore."
 *     As you look back at your modern JVM language with its dynamic typing and
 * low overhead closures you can't help but feel a sort of melancholy -
 * a longing for distant times.
 */
public final class Bootstrapper {
    // used by Compiler.KEY_HANDLE
    public static Keyword keywordConstant(MethodHandles.Lookup lookup, String value, Class<?> type) {
        return Keyword.makeKeyword(value);
    }

    // unused
    public static Symbol symbolConstant(MethodHandles.Lookup lookup, String value, Class<?> type) {
        return Symbol.makeSymbol(value);
    }

    /**
     * InvokeDynamic bootstrap method for creating arbitrary length sequences. Returns a CallSite which
     * packs the number of arguments given in methodType into an ArraySequence.
     * Used by the Lateral compiler to generate efficient calls to Lateral's list function.
     * @param lookup Lookup handle given by the InvokeDynamic instruction
     * @param name Not used
     * @param methodType Expected type of the CallSite
     * @return A CallSite for creating new Sequences
     * @throws IllegalAccessException Should never be thrown as ArraySequence.makeList is public
     * @throws NoSuchMethodException Should never be thrown as long as ArraySequence.makeList exists
     */
    public static CallSite sequenceBuilder(MethodHandles.Lookup lookup, String name, MethodType methodType)
            throws IllegalAccessException, NoSuchMethodException {
        int params = methodType.parameterCount();
        MethodHandle base = lookup.findStatic(ArraySequence.class, "makeList",
                MethodType.methodType(Sequence.class, Object[].class));
        return new ConstantCallSite(base.asCollector(Object[].class, params));
    }
}
