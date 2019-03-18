#ifndef LATERAL_ERROR_H
#define LATERAL_ERROR_H

enum error_type {
    syntax_err,
    type_err
};

struct Error {
    char* message;
    enum error_type type;
};

#endif
