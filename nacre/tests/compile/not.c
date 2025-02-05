#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

typedef uint64_t Bool;

extern Bool test(Bool x);

int main() {
    int nb0 = test((Bool) 0);
    int nb1 = test((Bool) 1);
    int r = (nb0 && !nb1);
    //printf("Test: %s\n", r? "pass" : "fail");
    return !r;
}
