#ifndef LA_OBJECT_H
#define LA_OBJECT_H

typedef enum {
    symt,
    strt,
    intt,
    floatt,
    listt,
    natfnt,
    fnt,
    macrot,
    errt
} obj_type;

struct Func {
    struct List *args;
    struct Object *expr;
};

typedef struct List {
    struct Object *obj;
    struct List *next;
} List;

union Data {
    void *ptr;
    struct Func func;
    struct Object *(*fn_ptr)(struct List *);
    int int_val;
    float float_val;
};

typedef struct Object {
    obj_type type;
    union Data data;
    int marked;
} Object;

Object *obj_init(obj_type, union Data);
void obj_free(Object *);
Object *err_init(char *);

unsigned int obj_hash(Object *);
int obj_equals(Object *, Object *);
int obj_eq_sym(Object *, char *);
int obj_is_empty_list(Object *);

void obj_print(Object *);
void obj_debug(Object *);

#endif
