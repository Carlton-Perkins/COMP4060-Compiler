#include <stdio.h>

static int __read_c = 0;

void _print_int(int i) {
    printf("%d\n", i);
}

int _read_int(void) {
    return __read_c++;
}
