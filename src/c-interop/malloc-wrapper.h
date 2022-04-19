#ifndef MALLOC_WRAPPER_H_
#define MALLOC_WRAPPER_H_

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif
void *get_buffer(unsigned int size);
void print_buffer(void *buf);
int check_val(char *buf);
#ifdef __cplusplus
}
#endif

#endif // MALLOC_WRAPPER_H_
