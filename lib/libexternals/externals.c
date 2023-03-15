#define CAML_NAME_SPACE
#include <caml/fail.h>
#include <stdio.h>

int print(char* s)
{
     printf("%s", s);
     return 0;
}

int print_double(double f)
{
     printf("%f", f);
     return 0;
}

int failwith(char* msg)
{
     caml_failwith(msg);
     return 0;
}
