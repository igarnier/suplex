#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>


int8_t print_int64(int64_t X) {
     printf("%ld\n", X);
     return 0;
}

int8_t print_int32(int32_t X) {
     printf("%d\n", X);
     return 0;
}

int8_t print_int16(int16_t X) {
     printf("%d\n", X);
     return 0;
}

int8_t print_int8(int8_t X) {
     printf("%d\n", X);
     return 0;
}

int8_t print_float(float X) {
     printf("%f\n", X);
     return 0;
}

int8_t print_double(double X) {
     printf("%f\n", X);
     return 0;
}

int8_t print_bool(int X) {
     if(X) {
          printf("true\n");
     } else {
          printf("false\n");
     }
     return 0;
}

int8_t print_unit(int8_t unit) {
     printf("()\n");
     return 0;
}

char* instralloc(size_t size) {
     printf("allocated %ld bytes\n", size);
     return malloc(size);
}
