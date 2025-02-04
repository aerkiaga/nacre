#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

typedef uint64_t Bool;

struct AClosure {
    Bool (*f)(struct AClosure *self, Bool x);
};

extern Bool test(struct AClosure *self, Bool x);

int main() {
    struct AClosure *ac = (struct AClosure*) malloc(sizeof(struct AClosure*));
    ac->f = test;
    int nb0 = !ac->f(ac, (Bool) !0);
    int nb1 = !ac->f(ac, (Bool) !1);
    int r = (nb0 && !nb1);
    //printf("Test: %s\n", r? "pass" : "fail");
    return !r;
}
