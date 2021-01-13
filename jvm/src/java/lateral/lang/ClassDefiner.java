package lateral.lang;

public class ClassDefiner extends ClassLoader {
    private ClassDefiner() { ; }

    private Class<?> loadClass(byte[] classBytes) {
        return super.defineClass(null, classBytes, 0, classBytes.length);
    }

    /**
     * Lateral's hook to define new classes at runtime.
     * No manager is needed to keep track of cousin classes because all references
     * are resolved via invokedyamic and Environment's bootstrap method.
     * When there are no more references to the loaded class and its ClassDefiner, both will
     * be garbage collected.
     *
     * All classes will be loaded by the same ClassDefiner, allowing these sibling classes
     * to reference each other directly. The class represented by the first byte array will be returned.
     * @param classBytes An array of byte arrays containing a valid representation of a JVM class
     * @return The class object created from classBytes
     */
    public static Class<?> hotloadClasses(byte[] ... classBytes) {
        ClassDefiner classDefiner = new ClassDefiner();
        Class<?> topLevel = classDefiner.loadClass(classBytes[0]);
        for(int i = 1; i < classBytes.length; i ++) {
            classDefiner.loadClass(classBytes[i]);
        }
        return topLevel;
    }
}
