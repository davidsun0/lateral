#ifndef LATERAL_OBJECT_H
#define LATERAL_OBJECT_H

enum object_type {empty, symbol, list_type, character, integer, string};

union Data {
    void* ptr;
    char character;
    int integer;
};

struct Object {
    enum object_type type;
    union Data data;
};

int object_equals_char(struct Object*, char);

void object_free_member(struct Object*);
void object_free(struct Object*);

void object_print(struct Object*);
void object_print_debug(struct Object*);

#endif
