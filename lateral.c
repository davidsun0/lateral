#include <stdlib.h>
#include <stdio.h>

#include <readline/readline.h>
#include <readline/history.h>

char* lread(){
    char* str = readline("user> ");
    return str;
}

char* leval(char* str){
    return str;
}

int lprint(char* str){
    if(str == NULL){
        printf("\ngoodbye (^_^ )/\"\n");
        return 0;
    }
    else{
        printf("%s\n", str);
        free(str);
        return 1;
    }
}

int rep(){
    return lprint(leval(lread()));
}

int main(){
    while(rep()){
        ;
    }
}
