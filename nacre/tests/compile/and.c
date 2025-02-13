#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>

extern bool test(bool x, bool y);

int main() {
    int ab00 = test(false, false);
    int ab01 = test(false, true);
    int ab10 = test(true, false);
    int ab11 = test(true, true);
    int r = (!ab00 && !ab01 && !ab10 && ab11);
    return !r;
}
