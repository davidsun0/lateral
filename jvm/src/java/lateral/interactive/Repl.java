package lateral.interactive;

import lateral.lang.Compiler;
import lateral.lang.LispReader;

import java.util.NoSuchElementException;
import java.util.Scanner;

public class Repl {
    public static void main(String[] args) {
        Compiler.load("./src/lisp/core.lisp");
        Scanner scanner = new Scanner(System.in);
        String next;
        System.out.println("Lateral Repl");
        while(true) {
            System.out.print("> ");
            try {
                next = scanner.nextLine();
            } catch (NoSuchElementException e) {
                // Ctrl+D
                break;
            }
            if(next.isEmpty())
                continue;
            System.out.print("=> ");
            try {
                System.out.println(Compiler.eval(LispReader.read(next)));
            } catch (Exception | Error e) {
                e.printStackTrace();
            }
        }
        System.out.println("Goodbye! (^_^ ) /");
    }
}
