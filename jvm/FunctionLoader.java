class FunctionLoader extends ClassLoader {

    private ClassLoader parent = FunctionLoader.class.getClassLoader();

    public Class<?> defineClass(byte[] bytes) {
        Class c = defineClass(null, bytes, 0, bytes.length);
        // resolveClass(c);
        return c;
    }

    @Override
    public Class<?> loadClass (String name) 
        throws ClassNotFoundException {
        System.out.println("Function Loader : " + name);
        /*
        if(name.startsWith("java")) {
            return parent.loadClass(name);
        }
        */

        return parent.loadClass(name);
    }
}
