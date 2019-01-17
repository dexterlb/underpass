#include <stdio.h>
#include <stdlib.h>
#include "wn.h"

int main(int argc, char* argv[]) {
    if (wninit()) {
        fprintf(stderr, "cannot open db\n");
        exit(1);
    }

    for (int pos = 1; pos <= NUMPARTS; pos++) {
        printf("pos %d (%s) :", pos, partnames[pos]);
        char* morph = morphstr(argv[1], pos);
        while (morph != NULL) {
            printf(" %s", morph);
            morph = morphstr(NULL, pos);
        }
        printf("\n");
    }
    return 0;
}
