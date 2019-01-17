#include <stdbool.h>
#include <stdio.h>
#include "wn.h"

bool wn_init_wordnet() {
    if (OpenDB) {
        return true;
    }

    if (wninit()) {
        return false;
    } else {
        return true;
    }
}
