#ifndef LA_READER_H
#define LA_READER_H

#include "object.h"

int read_token(char **, Object **);
int read_form(Object**, Object **);
Object* read_string(char *str);

#endif
