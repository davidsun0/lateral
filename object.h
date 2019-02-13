#ifndef LATERAL_OBJECT_H
#define LATERAL_OBJECT_H

struct List;

enum object_type {
    symbol,
    nil, true,
    char_type, string,
    int_type, float_type,
    list_type,
    c_fn,                           // pointer to function defined in C
    func_type,                      // lisp function
    macro_type
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
    int marked;
};

struct Object* object_init(enum object_type, union Data);
struct Object* object_symbol_init(char*);
struct Object* object_copy(struct Object*);

int object_equals_char(struct Object*, char);
int object_equals_symbol(struct Object*, char*);
int object_equals_string(struct Object*, char*);
int object_equals_value(struct Object*, struct Object*);
int object_is_nonempty_list(struct Object*);

void object_free(struct Object*);

void object_print_type(enum object_type);
void object_print_string(struct Object*);
void object_print_debug(struct Object*, int);
void object_debug(struct Object*);

void object_mark(struct Object*);

#endif
