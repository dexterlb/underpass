#include <stdbool.h>
#include "wn.h"

bool wn_init_wordnet() {
    if (OpenDB) {
        return true;
    }

    return !wninit();
}
