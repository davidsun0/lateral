#ifndef LA_OBJECT_H
#define LA_OBJECT_H

#define CAR(a) ((a)->data.cell.car)
#define CDR(a) ((a)->data.cell.cdr)

typedef enum {
    empty,
    symt,
    strt,
    keywordt,
    intt,
    floatt,
    listt,
    natfnt,
    fnt,
    macrot,
    errt
} obj_type;

struct Func {
    struct Object *args;
    struct Object *expr;
};

typedef struct Cell {
    struct Object *car;
    struct Object *cdr;
} Cell;

union Data {
    void *ptr;
    char str[16];
    int int_val;
    float float_val;
    struct Func func;
    Cell cell;
    struct Object *(*fn_ptr)(struct Object *);
};

typedef struct Object {
    obj_type type;
    union Data data;
    int marked;
} Object;

Object *nil_obj;
#define NIL (nil_obj)

Object *obj_init(obj_type, union Data);
void obj_free(Object *);
void obj_release(Object *);
void obj_mark(Object *);

Object *err_init(char *);
Object *str_init_len(int len, char *);
Object *str_init(char *);
Object *cell_init();

unsigned int obj_hash(Object *);
int obj_equals(Object *, Object *);
int obj_eq_sym(Object *, char *);

int obj_is_empty_list(Object *);
Object *list_length(Object *);
Object *list_append(Object * list, Object *);

void obj_print(Object *, int pretty);
void obj_debug(Object *);

#endif
