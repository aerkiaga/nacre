#include <stdio.h>
#include <stdlib.h>

struct Closure {
    struct Closure *(*f)(struct Closure *self, struct Closure *x);
    struct Closure *c[];
};

typedef struct Closure *Bool;

struct Closure *true_def2(struct Closure *self, struct Closure *x) {
    return self->c[0];
}

struct Closure *true_def(struct Closure *self, struct Closure *x) {
    struct Closure *r = (struct Closure*) malloc(2 * sizeof(struct Closure*));
    r->f = true_def2;
    r->c[0] = x;
    return r;
}

struct Closure *false_def2(struct Closure *self, struct Closure *x) {
    return x;
}

struct Closure *false_def(struct Closure *self, struct Closure *x) {
    struct Closure *r = (struct Closure*) malloc(sizeof(struct Closure*));
    r->f = false_def2;
    return r;
}

Bool into_bool(int x) {
    struct Closure *r = (struct Closure*) malloc(sizeof(struct Closure*));
    r->f = x? true_def : false_def;
    return r;
}

int from_bool(Bool x) {
    x = x->f(x, (void*) 1);
    x = x->f(x, (void*) 0);
    return (int) (long) x;
}

extern Bool test_a(struct Closure *self, Bool x);

int main() {
    struct Closure *ac = (struct Closure*) malloc(sizeof(struct Closure*));
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
