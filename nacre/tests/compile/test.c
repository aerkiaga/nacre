#include <stdio.h>
#include <stdlib.h>

struct BoolClosure {
    void *(*f)(struct BoolClosure *self, void *t, void *f);
};

typedef struct BoolClosure *Bool;

void *true_def(struct BoolClosure *self, void *t, void *f) {
    return t;
}

void *false_def(struct BoolClosure *self, void *t, void *f) {
    return f;
}

Bool into_bool(int x) {
    struct BoolClosure *r = (struct BoolClosure*) malloc(sizeof(struct BoolClosure*));
    r->f = x? true_def : false_def;
    return r;
}

int from_bool(Bool x) {
    return (int) (long) x->f(x, (void*) 1, (void*) 0);
}

struct AClosure {
    Bool (*f)(struct AClosure *self, Bool x);
};

extern Bool test_a(struct AClosure *self, Bool x);

int main() {
    struct AClosure *ac = (struct AClosure*) malloc(sizeof(struct AClosure*));
    ac->f = test_a;
    Bool b0 = into_bool(0);
    int i0 = from_bool(b0);
    Bool b1 = into_bool(1);
    int i1 = from_bool(b1);
    Bool nb0 = ac->f(ac, b0);
    int ni0 = from_bool(nb0);
    Bool nb1 = ac->f(ac, b1);
    int ni1 = from_bool(nb1);
    int r = (i0 == ni1 && i1 == ni0);
    printf("Test: %s\n", r? "pass" : "fail");
    return !r;
}
