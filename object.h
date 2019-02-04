#ifndef LATERAL_OBJECT_H
#define LATERAL_OBJECT_H

enum object_type {empty, symbol, character, integer, string};

union Data {
    void* ptr;
    char character;
    int integer;
};

struct Object {
    enum object_type type;
    union Data data;
};

void object_free_member(struct Object*);
void object_free(struct Object*);
void object_print_debug(struct Object*);

#endif
