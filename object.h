#ifndef LATERAL_OBJECT_H
#define LATERAL_OBJECT_H

enum object_type {empty,            // not nil, for internal use only
    symbol,
    list_type,
    char_type, string,
    int_type, float_type,
    c_fn                            // pointer to function defined in C
};

union Data {
    void* ptr;
    struct Object* (*fn_ptr)(struct Object*, struct Object*);
    char char_type;
    int int_type;
};

struct Object {
    enum object_type type;
    union Data data;
};

struct Object* object_init(enum object_type, union Data);

int object_equals_char(struct Object*, char);

void object_free_member(struct Object*);
void object_free(struct Object*);

void object_print(struct Object*);
void object_print_string(struct Object*);
void object_print_debug(struct Object*);

#endif
