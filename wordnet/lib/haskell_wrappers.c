#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

#include "wn.h"

bool wn_init_wordnet(char* wn_dir) {
    setenv("WNSEARCHDIR", wn_dir, 1);

    if (OpenDB) {
        return true;
    }

    return (!wninit());
}
