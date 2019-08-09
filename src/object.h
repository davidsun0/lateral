#ifndef LA_OBJECT_H
#define LA_OBJECT_H

typedef enum {
    empty = 0,
    // string variety
    symt, strt, keywordt,
    // numerical variety
    intt, floatt,
    // data structure variety
    listt, hashmapt,
    // function variety
    natfnt, fnt, macrot,

    errt
} obj_type;

struct Func {
    struct Object *params;
    struct Object *expr;
};

typedef struct Cell {
    struct Object *car;
    struct Object *cdr;
} Cell;

#define CAR(a) ((a)->data.cell.car)
#define CDR(a) ((a)->data.cell.cdr)

typedef struct {
    struct Object *buckets;
    int capacity;
    int load;
} HashMap;

#define SHORT_STRING_LENGTH 16

union Data {
    void *ptr;
    char *str;
    char short_str[SHORT_STRING_LENGTH];
    HashMap *hashmap;
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

// natural numbers indicating number of bits to shift to get flag
#define MARK_MASK 1
#define SSTR_MASK 2

#define SET_MARK(a) ((a)->flags |= (1 << MARK_MASK))
#define UNSET_MARK(a) ((a)->flags &= ~(1 << MARK_MASK))
#define GET_MARK(a) ((a)->flags & (1 << MARK_MASK))

#define SET_SSTR(a) ((a)->flags |= (1 << SSTR_MASK))
#define GET_SSTR(a) ((a)->flags & (1 << SSTR_MASK))

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
Object *obj_hashmap_init(int);

void obj_print(Object *, int pretty);
void obj_debug(Object *);

int obj_equals(Object *, Object *);
int obj_eq_sym(Object *, char *);

unsigned int str_hash(char *);
unsigned int obj_hash(Object *);

int list_length(Object *);
Object *list_append(Object * list, Object *);

HashMap *hashmap_init(int size);
void hashmap_free(HashMap *);
void hashmap_resize(HashMap *);

void hashmap_set(HashMap *, Object *key, Object *value);
Object *hashmap_get(HashMap *, Object *key);
int hashmap_rem(HashMap *, Object *key);

void hashmap_print(HashMap *, int);
void hashmap_debug(HashMap *);

typedef struct Envir {
    HashMap *map;
    struct Envir *prev;
    struct Envir *next;
} Envir;

Envir *envir_init(int size);
void envir_free(Envir *);

void envir_set(Envir *, Object *key, Object *value);
void envir_set_str(Envir *, char *key, Object *value);
Object *envir_get(Envir *, Object *key);
Object *envir_search(Envir *, Object *key);
#endif
