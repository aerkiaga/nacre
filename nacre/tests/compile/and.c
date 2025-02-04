#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

typedef uint64_t Bool;

struct AClosure {
    Bool (*f)(struct AClosure *self, Bool x, Bool y);
};

extern Bool test(struct AClosure *self, Bool x, Bool y);

int main() {
    struct AClosure *ac = (struct AClosure*) malloc(sizeof(struct AClosure*));
    ac->f = test;
    int ab00 = ac->f(ac, (Bool) 0, (Bool) 0);
    int ab01 = ac->f(ac, (Bool) 0, (Bool) 1);
    int ab10 = ac->f(ac, (Bool) 1, (Bool) 0);
    int ab11 = ac->f(ac, (Bool) 1, (Bool) 1);
    int r = (!ab00 && !ab01 && !ab10 && ab11);
    //printf("Test: %s\n", r? "pass" : "fail");
    return !r;
}
