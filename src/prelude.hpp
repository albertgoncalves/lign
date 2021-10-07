#ifndef __PRELUDE_H__
#define __PRELUDE_H__

#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

typedef int32_t i32;

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

#define EXIT()                                                       \
    {                                                                \
        fprintf(stderr, "%s:%s:%d\n", __FILE__, __func__, __LINE__); \
        _exit(EXIT_FAILURE);                                         \
    }

#define EXIT_WITH(x)                                                         \
    {                                                                        \
        fprintf(stderr, "%s:%s:%d `%s`\n", __FILE__, __func__, __LINE__, x); \
        _exit(EXIT_FAILURE);                                                 \
    }

#define EXIT_IF(condition)     \
    if (condition) {           \
        EXIT_WITH(#condition); \
    }

#endif
