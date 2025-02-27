#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>

struct PairBool {
    bool left;
    bool right;
};

extern struct PairBool test(bool x, bool y);

int main() {
    struct PairBool pb00 = test(false, false);
    struct PairBool pb01 = test(false, true);
    struct PairBool pb10 = test(true, false);
    struct PairBool pb11 = test(true, true);
    bool r = !pb00.left && !pb00.right && !pb01.left && pb01.right && pb10.left && !pb10.right && pb11.left && pb11.right;
    return !r;
}
