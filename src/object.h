#ifndef LA_OBJECT_H
#define LA_OBJECT_H

#define CAR(a) ((a)->data.cell.car)
#define CDR(a) ((a)->data.cell.cdr)

// natural numbers indicating number of bits to shift to get flag
#define MARK_MASK 1
#define SSTR_MASK 2

#define SET_MARK(a) ((a)->flags |= (1 << MARK_MASK))
#define UNSET_MARK(a) ((a)->flags &= ~(1 << MARK_MASK))
#define GET_MARK(a) ((a)->flags & (1 << MARK_MASK))

#define SET_SSTR(a) ((a)->flags |= (1 << SSTR_MASK))
#define GET_SSTR(a) ((a)->flags & (1 << SSTR_MASK))

typedef enum {
    empty = 0,
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
    char *str;
    char short_str[16];
    int int_val;
    float float_val;
    struct Func func;
    Cell cell;
    struct Object *(*fn_ptr)(struct Object *);
};

typedef struct Object {
    obj_type type;
    int flags;
    union Data data;
} Object;

Object *nil_obj;
#define NIL (nil_obj)

char *type_to_str(obj_type);

char *obj_string(Object *);

Object *obj_init(obj_type, union Data);
Object *obj_init_str_len(obj_type, char *str, int len);
Object *obj_init_str(obj_type, char *str);
void obj_release(Object *);
void obj_mark(Object *);

Object *err_init(char *);
Object *str_init_len(int len, char *);
Object *str_init(char *);
Object *cell_init();

unsigned int obj_hash(Object *);
int obj_equals(Object *, Object *);
int obj_eq_sym(Object *, char *);

Object *list_length(Object *);
Object *list_append(Object * list, Object *);

void obj_print(Object *, int pretty);
void obj_debug(Object *);

#endif
