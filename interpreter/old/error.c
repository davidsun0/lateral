#include <stdlib.h>
#include <stdio.h>

#include "object.h"

#include "error.h"

struct Object* error_init_bare() {
    // return object_init_type(error_type);
    return error_init(generic_err, "an error occured");
}

struct Object* error_init(enum error_type type, const char* msg) {
    struct Object* obj = object_init_type(error_type);
    obj->data.ptr = malloc(sizeof(struct Error));
    struct Error* err = obj->data.ptr;
    err->message = msg;
    err->type = type;
    err->freeable = 0;
    return obj;
}
