#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>

struct PairBool {
    bool left;
    bool right;
};

extern bool test(struct PairBool p);

int main() {
    struct PairBool pb00 = {
        false,
        false,
    };
    bool b0a = test(pb00);
    struct PairBool pb01 = {
        false,
        true,
    };
    bool b0b = test(pb01);
    struct PairBool pb10 = {
        true,
        false,
    };
    bool b1a = test(pb10);
    struct PairBool pb11 = {
        true,
        true,
    };
    bool b1b = test(pb11);
    bool r = !b0a && !b0b && b1a && b1b;
    return !r;
}
