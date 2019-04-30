#ifndef LATERAL_ERROR_H
#define LATERAL_ERROR_H

enum error_type {
    generic_err,
    syntax_err,
    type_err,
    name_err,
    arg_err
};

struct Error {
    const char* message;
    enum error_type type;
    int freeable;
};

struct Object* error_init_bare();

struct Object* error_init(enum error_type, const char*);

#endif
