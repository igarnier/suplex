#define CAML_NAME_SPACE
#include <caml/fail.h>

int failwith(char* msg)
{
     caml_failwith(msg);
     return 0;
}
