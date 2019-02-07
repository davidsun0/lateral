#ifndef LATERAL_OBJECT_H
#define LATERAL_OBJECT_H

struct List;

enum object_type {
    symbol,
    list_type,
    char_type, string,
    int_type, float_type,
    c_fn,                           // pointer to function defined in C
    func_type                       // lisp function
};

enum quote {
    none, semi, full
};

struct Func {
    struct List* args;
    struct Object* expr;
};

union Data {
    void* ptr;
    struct Func* func;
    struct Object* (*fn_ptr)(struct List*);
    char char_type;
    int int_type;
};

struct Object {
    enum object_type type;
    union Data data;
    enum quote quote;
};

struct Object* object_init(enum object_type, union Data);
struct Object* object_copy(struct Object*);

int object_equals_char(struct Object*, char);
int object_equals_symbol(struct Object*, char*);

void object_free_member(struct Object*);
void object_free(struct Object*);

void object_print_string(struct Object*);
void object_print_debug(struct Object*);

#endif
