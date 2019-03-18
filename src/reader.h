#ifndef LATERAL_READER_H
#define LATERAL_READER_H

#include "object.h"

struct Object* read_string(char* str);

struct Object* read_module(char* filename);

#endif
