#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>

extern bool test(bool x);

int main() {
    int nb0 = test(false);
    int nb1 = test(true);
    int r = (nb0 && !nb1);
    return !r;
}
