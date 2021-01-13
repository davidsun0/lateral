class FunctionLoader extends ClassLoader {
    public Class<?> defineClass(byte[] bytes) {
        Class c = defineClass(null, bytes, 0, bytes.length);
        return c;
    }
}
