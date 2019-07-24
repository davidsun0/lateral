#ifndef LA_READER_H
#define LA_READER_H

#include "object.h"
#include "list.h"

char *la_strdup(char *str);
int read_token(char **, Object **);
int read_form(List **, Object **);
Object* read_string(char *str);

#endif
