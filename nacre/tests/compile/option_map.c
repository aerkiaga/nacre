#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>

struct OptionBool {
    bool tag;
    bool value;
};

struct BoolMap {
    bool (*f)(struct BoolMap*, bool);
};

extern struct OptionBool test(struct BoolMap *f, struct OptionBool x);

bool not(struct BoolMap *self, bool x) {
    return !x;
}

int main() {
    struct BoolMap not_map = {
        .f = not,
    };
    struct OptionBool none = {
        .tag = false,
        .value = false,
    };
    struct OptionBool none_map = test(&not_map, none);
    struct OptionBool some_false = {
        .tag = true,
        .value = false,
    };
    struct OptionBool some_false_map = test(&not_map, some_false);
    struct OptionBool some_true = {
        .tag = true,
        .value = true,
    };
    struct OptionBool some_true_map = test(&not_map, some_true);
    bool r = !none_map.tag && some_false_map.tag && some_false_map.value && some_true_map.tag && !some_true_map.value;
    return !r;
}
