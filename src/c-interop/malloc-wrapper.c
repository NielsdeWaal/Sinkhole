#include "malloc-wrapper.h"
#include <stdio.h>
#include <string.h>

void *get_buffer(unsigned int size) {
  char *temp = (char *)malloc(size);
  /* temp[0] = "A"; */
  return temp;
}

void print_buffer(void *buf) { printf("%s\n", (char *)buf); }

int check_val(char *buf) { return strcmp(buf, "a"); }
