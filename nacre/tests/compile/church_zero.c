#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>

struct Church {
    bool tag;
    struct Church *next;
};

extern struct Church test(struct Church n);

int main() {
    struct Church invalid = {
        true,
        NULL,
    };
    struct Church zero = test(invalid);
    bool r = !zero.tag;
    return !r;
}
