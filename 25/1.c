#include <stdio.h>

int main() {
    // int input[2] = { 5764801, 17807724 };
    int input[2] = { 8335663, 8614349 };
    
    unsigned long i = 0;
    unsigned long n = 1;
    unsigned long bn = -1;
    long a = -1;
    long b = -1;
    while (b == -1) {
        i++;
        n *= 7;
        n %= 20201227l;

        if (n == input[0]) {
            a = i;
        }
        if (n == input[1]) {
            b = i;
            bn = n;
        }
    }

    n = 1;
    for (int j = 0; j < a; j++) {
        n *= bn;
        n %= 20201227l;
    }

    printf("%lu\n",n);
    return 0;
}