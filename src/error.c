#include <stdlib.h>
#include <stdio.h>

#include "object.h"

#include "error.h"

struct Object* error_init() {
    return object_init_type(error_type);
}
