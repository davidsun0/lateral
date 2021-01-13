package lateral.lang;

import org.objectweb.asm.Type;

import java.io.IOException;
import java.lang.invoke.CallSite;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayDeque;
import java.util.ArrayList;

public class Compiler {
    static Symbol LAMBDA = Symbol.makeSymbol("function");
    static Symbol DEFMACRO = Symbol.makeSymbol("defmacro");
    static Symbol DEFUN = Symbol.makeSymbol("defun");
    static Symbol DEFINE = Symbol.makeSymbol("def");
    static Symbol ASM = Symbol.makeSymbol("asm-quote");
    static Symbol DEASM = Symbol.makeSymbol("asm-unquote");
    static Symbol LET = Symbol.makeSymbol("let");
    static Symbol IF = Symbol.makeSymbol("if");
    static Symbol QUOTE = Symbol.makeSymbol("quote");
    static Symbol LIST = Symbol.makeSymbol("list");
    static Symbol RECUR = Symbol.makeSymbol("recur");

    static Keyword REST = Keyword.makeKeyword("rest");

    static Sequence PARSE_INT = new ArraySequence(
            Assembler.INVOKESTATIC, Type.getInternalName(Integer.class),
            "valueOf", Assembler.getMethodDescriptor(int.class, Integer.class)
    );

    // TODO: convert to dynamic LDC
    static Sequence MAKE_SYM = new ArraySequence(
            Assembler.INVOKESTATIC, Type.getInternalName(Symbol.class),
            "makeSymbol", Assembler.getMethodDescriptor(String.class, Symbol.class)
    );

    static Sequence KEY_HANDLE = Sequence.makeList(
            Type.getInternalName(Bootstrapper.class), "keywordConstant",
            MethodType.methodType(Keyword.class, MethodHandles.Lookup.class,
                    String.class, Class.class).toMethodDescriptorString());

    // Environment.dynamicObject
    // TODO: convert to dynamic LDC
    static Sequence ENVIR_OBJECT = new ArraySequence(
            Type.getInternalName(Environment.class), "dynamicObject",
            MethodType.methodType(CallSite.class, MethodHandles.Lookup.class, String.class,
                    MethodType.class, String.class).toMethodDescriptorString()
    );

    // Environment.dynamicFunction
    static Sequence ENVIR_FUNCTION = new ArraySequence(
            Type.getInternalName(Environment.class), "dynamicFunction",
            MethodType.methodType(CallSite.class, MethodHandles.Lookup.class, String.class,
                    MethodType.class, String.class).toMethodDescriptorString()
    );

    // Bootstrapper.sequenceBuilder
    static Sequence SEQUENCE_BOOTSTRAP = new ArraySequence(
            Type.getInternalName(Bootstrapper.class), "sequenceBuilder",
            MethodType.methodType(CallSite.class, MethodHandles.Lookup.class, String.class,
                    MethodType.class).toMethodDescriptorString()
    );

    ArrayList<CompClass> classes = new ArrayList<>();

    public static Object macroExpand(Object expr) {
        while(true) {
            if(!(expr instanceof Sequence)) {
                return expr;
            }
            Object head = ((Sequence) expr).first();
            if(!(head instanceof Symbol)) {
                return expr;
            }
            Object resource = Environment.getIfExists((Symbol) head);
            if(resource instanceof Function && ((Function) resource).isMacro()) {
                Function macro = (Function) resource;
                Object[] args = new Object[((Sequence) expr).rest().size()];
                // ignore the first arg, which is the macro name
                Sequence argSeq = ((Sequence) expr).rest();
                for(int i = 0; i < args.length; i ++) {
                    args[i] = argSeq.first();
                    argSeq = argSeq.rest();
                }
                expr = macro.apply(args);
            } else {
                return expr;
            }
        }
    }

    static class CompilationContext {
        ArrayList<Object> bytecode;
        CompEnvir envir;
        CompClass compClass;
        boolean isVarargs;
        int paramCount;

        CompilationContext(CompEnvir envir, CompClass compClass, int paramCount, boolean isVarargs) {
            bytecode = new ArrayList<>();
            this.envir = envir;
            this.compClass = compClass;
            this.paramCount = paramCount;
            this.isVarargs = isVarargs;
        }

        void add(Object ... objects) {
            if(objects == null || objects.length == 0) {
                bytecode.add(EmptySequence.EMPTY_SEQUENCE);
            } else if(objects.length == 1) {
                bytecode.add(objects[0]);
            } else {
                bytecode.add(new ArraySequence(objects));
            }
        }
    }

    void compileQuote(Object ast, CompilationContext context) {
        if(ast instanceof Symbol) {
            context.add(Assembler.LDC, ast.toString());
            context.add(MAKE_SYM);
        } else if(ast instanceof Keyword) {
            context.add(Assembler.LDC,
                    Sequence.makeList(
                            ((Keyword) ast).getValue(),
                            Type.getDescriptor(Keyword.class),
                            KEY_HANDLE));
        } else if(ast instanceof Integer) {
            context.add(Assembler.ICONST, ast);
            context.add(PARSE_INT);
        } else if(ast instanceof String) {
            context.add(Assembler.LDC, ast);
        } else if(ast instanceof Sequence) {
            Sequence body = (Sequence) ast;
            for(Object obj : body) {
                compileQuote(obj, context);
            }
            context.add(Assembler.INVOKEDYNAMIC, SEQUENCE_BOOTSTRAP,
                    "makeSequence", Assembler.getMethodDescriptor(Sequence.class, body.size()));
        } else {
            throw new RuntimeException("can't quote " + ast);
        }
    }

    CompClass compileLambda(Sequence expr, CompEnvir parentEnvir) {
        Object head = expr.first();
        Sequence forms = expr.rest();
        boolean isMacro = false;
        Symbol name = null;
        if(DEFUN.equals(head)) {
            name = (Symbol) expr.second();
            forms = forms.rest();
        } else if(DEFMACRO.equals(head)) {
            name = (Symbol) expr.second();
            isMacro = true;
            forms = forms.rest();
        }

        CompClass lambdaClass = new CompClass(name);
        classes.add(lambdaClass);

        // TODO: assert that there are a matched number of params / body
        for(; !forms.isEmpty(); forms = forms.rest().rest()) {
            CompEnvir lambdaEnvir = new CompEnvir(parentEnvir, lambdaClass);
            Sequence params = (Sequence) forms.first();
            int paramCount = params.size();
            boolean isVarargs = false;
            // TODO: destructuring
            Sequence destruct = params;
            while (!destruct.isEmpty()) {
                if (REST.equals(destruct.first()) && destruct.rest().rest().isEmpty()) {
                    lambdaEnvir.insert((Symbol) destruct.second());
                    isVarargs = true;
                    paramCount--;
                    break;
                } else {
                    lambdaEnvir.insert((Symbol) destruct.first());
                }
                destruct = destruct.rest();
            }
            CompilationContext context = new CompilationContext(lambdaEnvir, lambdaClass, paramCount, isVarargs);
            // compile invoke method
            compile(forms.second(), context, true);
            lambdaClass.generateInvoker(paramCount, isVarargs, context.bytecode);
        }

        // then generate class methods (depends on body compilation)
        lambdaClass.generateConstructor();
        // lambdaClass.generateInherits(isMacro, isVarargs, paramCount);
        lambdaClass.generateInherits(isMacro);
        if(name != null)
            lambdaClass.generateToString(name.toString());
        return lambdaClass;
    }

    void compile(Object ast, CompilationContext context, boolean isTail) {
        ast = macroExpand(ast);
        if(ast instanceof Sequence) {
            Sequence astSequence = (Sequence) ast;
            Object head = astSequence.first();
            Sequence body = astSequence.rest();
            if (LAMBDA.equals(head)) {
                // compile body
                // need: constructor args
                // need: constructor
                CompClass lambdaClass = compileLambda((Sequence) ast, context.envir);
                context.add(Assembler.NEW, lambdaClass.getClassName());
                context.add(Assembler.DUP);
                for(Symbol sym : lambdaClass.getCaptured()) {
                    compile(sym, context, false);
                }
                context.add(Assembler.INVOKESPECIAL, lambdaClass.getClassName(), "<init>",
                        lambdaClass.getConstructor());
            } else if (LET.equals(head)) {
                CompEnvir parentEnvir = context.envir;
                context.envir = new CompEnvir(context.envir);
                Sequence bindings = (Sequence) body.first();
                while (!bindings.isEmpty()) {
                    compile(bindings.second(), context, false);
                    int index = context.envir.insert((Symbol) bindings.first());
                    context.add(Sequence.makeList(Assembler.ASTORE, index));
                    bindings = bindings.rest().rest();
                }
                compile(body.second(), context, isTail);
                context.envir = parentEnvir;
                return;
            } else if (RECUR.equals(head)) {
                /*
                assert that
                - in tail position
                - args applicable to current function
                - inside function construct
                pseudocode:
                if first of bytecodes is not label:
                    gensym label
                compile and store each argument
                goto label
                 */
                if(!isTail || (context.isVarargs && body.size() < context.paramCount - 1)
                    || (!context.isVarargs && body.size() != context.paramCount)) {
                    throw new RuntimeException("wrong arity for recur");
                }
                Symbol label;
                Object firstBytecode = context.bytecode.get(0);
                if(firstBytecode instanceof Sequence &&
                        Assembler.LABEL.equals(((Sequence) firstBytecode).first())) {
                    label = (Symbol) ((Sequence) firstBytecode).second();
                } else {
                    label = Symbol.gensym("start");
                    context.bytecode.add(0, Sequence.makeList(Assembler.LABEL, label));
                }

                for(Object obj : body) {
                    compile(obj, context, false);
                }
                // first slot is 'this', locals begin at 1
                for(int i = body.size(); i > 0; i --) {
                    context.add(Assembler.ASTORE, i);
                }
                // TODO: repack rest arguments if function is varargs
                context.add(Assembler.GOTO, label);
                return;
            } else if (QUOTE.equals(head)) {
                compileQuote(body.first(), context);
            } else if (LIST.equals(head)) {
                for (Object arg : astSequence.rest()) {
                    compile(arg, context, false);
                }
                context.add(Assembler.INVOKEDYNAMIC, SEQUENCE_BOOTSTRAP,
                        "makeSequence", Assembler.getMethodDescriptor(Sequence.class, body.size()));
            } else if (IF.equals(head)) {
                // TODO: assert arglen = 3
                Symbol targetLabel = Symbol.gensym("if");
                Symbol endLabel = null;

                // test clause
                compile(body.first(), context, false);
                context.add(Assembler.IFNULL, targetLabel);
                // then clause
                compile(body.second(), context, isTail);
                if (!isTail) {
                    endLabel = Symbol.gensym("end");
                    context.add(Assembler.GOTO, endLabel);
                }
                context.add(Assembler.LABEL, targetLabel);
                // else clause
                compile(body.third(), context, isTail);
                if (!isTail) {
                    context.add(Assembler.LABEL, endLabel);
                }
                // skip end of method's isTail test
                return;
            } else if (ASM.equals(head)) {
                for (Object obj : astSequence.rest()) {
                    if (obj instanceof Sequence && DEASM.equals(((Sequence) obj).first()))
                        compile(((Sequence) obj).second(), context, false);
                    else
                        context.add(obj);
                }
            } else if (head instanceof Symbol && !context.envir.contains((Symbol) head)) {
                if(head.equals(context.compClass.getFunctionName())) {
                    /*
                    Optimization: if the function calls itself with a different arity,
                    bypass the apply function and directly call invoke
                    TODO: extend to lambdas calling parent function
                     */
                    context.add(Assembler.ALOAD, 0);
                    for (Object arg : body) {
                        compile(arg, context, false);
                    }
                    context.add(Assembler.INVOKEVIRTUAL, context.compClass.getClassName(),
                            "invoke", Assembler.getMethodDescriptor(Object.class, body.size()));
                } else {
                    // optimization via invokedynamic on function name
                    for (Object arg : body) {
                        compile(arg, context, false);
                    }
                    context.add(Assembler.INVOKEDYNAMIC, ENVIR_FUNCTION, "futureUse",
                            Assembler.getMethodDescriptor(Object.class, body.size()),
                            head.toString());
                }
            } else {
                // dynamically load function object and call Function.apply
                compile(head, context, false);
                // pack arguments into array
                int argc = body.size();
                context.add(Assembler.CHECKCAST, Type.getInternalName(Function.class));
                context.add(Assembler.ICONST, argc);
                context.add(Assembler.ANEWARRAY, Type.getInternalName(Object.class));
                for (int i = 0; i < argc; i++) {
                    context.add(Assembler.DUP);
                    context.add(Assembler.ICONST, i);
                    compile(body.first(), context, false);
                    context.add(Assembler.AASTORE);
                    body = body.rest();
                }
                // call apply
                context.add(Assembler.INVOKEVIRTUAL, Type.getInternalName(Function.class),
                        "apply", "([Ljava/lang/Object;)Ljava/lang/Object;");
            }
        } else if(ast instanceof Symbol) {
            /*
            resolve a symbol: determine if it is
            - local field (argument or let variable): aload
            - closed-over variable: get field
            - global variable: invokedynamic for object from envir
             */
            Symbol symAst = (Symbol) ast;
            CompEnvir compEnvir = context.envir;
            boolean closureSeen = false;
            boolean closedVariable = false;
            ArrayDeque<CompEnvir> envirChain = new ArrayDeque<>();
            while (compEnvir != null) {
                if (closureSeen)
                    closedVariable = true;
                if (compEnvir.closure != null)
                    closureSeen = true;
                envirChain.push(compEnvir);

                if (compEnvir.bindings.containsKey(symAst))
                    break;
                else
                    compEnvir = compEnvir.parent;
            }
            // searched through all parent envirs and did not find symbol
            if (compEnvir == null) {
                // if not in parents, invokedynamic to bind to global
                context.add(Assembler.INVOKEDYNAMIC, ENVIR_OBJECT, ast.toString(),
                        Assembler.getMethodDescriptor(Object.class, 0), "test");
            } else {
                CompEnvir top = envirChain.pop();
                if (closedVariable) {
                    // pull the closed variable through all of the closures
                    while (!envirChain.isEmpty()) {
                        compEnvir = envirChain.pop();
                        if (compEnvir.closure != null) {
                            compEnvir.closure.addCaptured(symAst);
                            // compEnvir.closure.captured.add(symAst);
                        }
                    }
                    // load the function object onto the stack
                    context.add(Assembler.ALOAD, 0);
                    // get the closed variable from the closed field
                    context.add(Assembler.GETFIELD, context.compClass.name, symAst.toString(),
                            Type.getDescriptor(Object.class));
                    // System.out.println("closed over var " + symAst.toString());
                } else {
                    int localSlot = top.bindings.get(symAst);
                    // System.out.println(symAst.toString() + " is localvar@" + localSlot);
                    context.add(Assembler.ALOAD, localSlot);
                }
            }
        } else if (ast instanceof Integer) {
            context.add(Assembler.ICONST, ast);
            context.add(PARSE_INT);
        } else if (ast instanceof String) {
            context.add(Assembler.LDC, ast);
        } else if (ast instanceof Keyword) {
            context.add(Assembler.LDC,
                    Sequence.makeList(
                            ((Keyword) ast).getValue(),
                            Type.getDescriptor(Keyword.class),
                            KEY_HANDLE));
        } else {
            throw new RuntimeException(ast.toString());
        }
        if(isTail)
            context.add(Assembler.ARETURN);
    }

    public static Object eval(Object ast) {
        ast = macroExpand(ast);
        Compiler compiler = new Compiler();
        if(ast instanceof Sequence) {
            Object head = ((Sequence) ast).first();
            if(DEFINE.equals(head)) {
                Symbol name = (Symbol) ((Sequence) ast).second();
                Object value = eval(((Sequence) ast).third());
                return Environment.insert(name, value);
            } else if(DEFMACRO.equals(head) || DEFUN.equals(head)) {
                Symbol name = (Symbol) ((Sequence) ast).second();
                compiler.compileLambda((Sequence) ast, null);
                byte[][] classBytes = new byte[compiler.classes.size()][];
                for(int i = 0; i < compiler.classes.size(); i ++) {
                    classBytes[i] = Assembler.buildClass(compiler.classes.get(i).toTree());
                }
                try {
                    Class<?> mainClass = ClassDefiner.hotloadClasses(classBytes);
                    Constructor<?> constructor = mainClass.getConstructor();
                    return Environment.insert(name, constructor.newInstance());
                } catch (NoSuchMethodException | InstantiationException |
                        IllegalAccessException | InvocationTargetException e) {
                    System.err.println("internal compiler error:");
                    e.printStackTrace();
                }
            }
        }

        CompClass main = new CompClass();
        compiler.classes.add(main);
        CompilationContext context = new CompilationContext(new CompEnvir(null), main, 0, false);
        compiler.compile(ast, context, true);
        main.generateInvoker(0, false, context.bytecode);
        main.generateInherits(false);
        main.generateConstructor();

        byte[][] classBytes = new byte[compiler.classes.size()][];
        for(int i = 0; i < compiler.classes.size(); i ++) {
            classBytes[i] = Assembler.buildClass(compiler.classes.get(i).toTree());
        }
        try {
            Class<?> mainClass = ClassDefiner.hotloadClasses(classBytes);
            Constructor<?> constructor = mainClass.getConstructor();
            Object object = constructor.newInstance();
            return ((Function) object).apply();
        } catch (NoSuchMethodException | InstantiationException |
                IllegalAccessException | InvocationTargetException e) {
            System.err.println("internal compiler error:");
            e.printStackTrace();
        }
        return null;
    }

    public static Object load(String filename) {
        try {
            LispReader lispReader = LispReader.fileReader(filename);
            Object form;
            while((form = lispReader.readForm()) != null) {
                eval(form);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void main(String[] args) throws IOException {
        load("./src/lisp/core.lisp");
        LispReader lispReader = LispReader.fileReader("./src/lisp/compiler.lisp");
        Object form;
        while((form = lispReader.readForm()) != null) {
            try {
                System.out.println("=> " + eval(form));
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}
