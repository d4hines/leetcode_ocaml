#include <stdio.h>
#include "mathlib.h"



typedef unsigned char *byte_pointer;

void show_bytes(byte_pointer start, int len) {
 int i;

 for (i = 0; i < len; i++)
   printf(" %.2x", start[i]);
   printf("\n");
}

void show_int(int x) {
 show_bytes((byte_pointer) &x, sizeof(int));
}

void show_float(float x) {
 show_bytes((byte_pointer) &x, sizeof(float));
}

void show_pointer(void *x) {
 show_bytes((byte_pointer) &x, sizeof(void *));
}


int main() {
    printf("The result of 3 + 4 is %d\n", add(3, 4));
    show_int(987654321);
    show_float(3.14);
    show_bytes("hello world", 11);
    return 0;
}
