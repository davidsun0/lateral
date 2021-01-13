package lateral.lang;

import java.util.HashMap;

/**
 * Lisp Environment for use in compilation
 */
class CompEnvir {
   CompEnvir parent;
   CompClass closure;
   HashMap<Symbol, Integer> bindings = new HashMap<>();
   int bindCount;

   CompEnvir(CompEnvir parent) {
       this(parent, null);
   }

   CompEnvir(CompEnvir parent, CompClass closure) {
       this.parent = parent;
       this.closure = closure;
       // restart locals if this is top envir or when making a closure
       if(parent == null || closure != null)
           // first local slot is taken by Function, symbols start at 1
           bindCount = 1;
       else
           bindCount = parent.bindCount;
   }

   int insert(Symbol symbol) {
       bindings.put(symbol, bindCount);
       bindCount ++;
       return bindCount - 1;
   }

   boolean contains(Symbol symbol) {
       CompEnvir envir = this;
       while(envir != null) {
           if(envir.bindings.containsKey(symbol))
               return true;
           envir = envir.parent;
       }
       return false;
   }
}
