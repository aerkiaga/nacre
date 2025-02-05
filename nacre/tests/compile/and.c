#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

typedef uint64_t Bool;

extern Bool test(Bool x, Bool y);

int main() {
    int ab00 = test((Bool) 0, (Bool) 0);
    int ab01 = test((Bool) 0, (Bool) 1);
    int ab10 = test((Bool) 1, (Bool) 0);
    int ab11 = test((Bool) 1, (Bool) 1);
    int r = (!ab00 && !ab01 && !ab10 && ab11);
    //printf("Test: %s\n", r? "pass" : "fail");
    return !r;
}
