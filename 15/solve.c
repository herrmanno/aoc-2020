/*
    compile:    `clang -O3 -o solve solve.c`
    run:        `./solve <n>`
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#define SHIFT 32

int occured_count (uint64_t o) {
    uint32_t o1 = o;
    uint32_t o2 = o >> SHIFT;
    if (o1 == 0 && o2 == 0) {
        return 0;
    } else if (o1 != 0 && o2 != 0) {
        return 2;
    } else {
        return 1;
    }
}

uint32_t diff_occurences(uint64_t o) {
    uint32_t o1 = o;
    uint32_t o2 = o >> 32;
    return o1 > o2 ? o1 - o2 : o2 - o1;
}

void push (uint64_t *arr, uint32_t i, uint32_t n) {
    uint32_t low = (arr[n] >> SHIFT);
    uint64_t high = ((uint64_t)i) << SHIFT;
    arr[n] = high + low;
}

int main(int argc, char** argv) {
    unsigned int start[] = {11,0,1,10,5,19};
    // uint64_t start[] = {0,3,6};
    int startSize = sizeof(start) / sizeof(start[0]);
    uint64_t last = start[startSize-1];
    uint64_t j = atoi(argv[1]);

    uint64_t *arr = malloc(j * sizeof(uint64_t));

    for (unsigned int i = 0; i < j; i++) {
        arr[i] = 0;
    }

    for (uint64_t i = 0; i < startSize; i++) {
        arr[start[i]] = ((i+1) << SHIFT) + (u_int32_t)arr[start[i]];
    }

    for (unsigned int i = startSize; i < j; i++) {
        // printf("last: %llu\t", last);

        uint64_t last_occurences = arr[last];
        // printf("occurences: %d,%d\t", (uint32_t)(last_occurences >> 32), (uint32_t)last_occurences);

        int occurences_size = occured_count(last_occurences);
        // printf("n_occurences: %d\n", occurences_size);

        switch (occurences_size) {
            case 0: exit(-1);
            case 1:
                last = 0;
                break;
            default:
                last = diff_occurences(last_occurences);
                break;
        }

        push(arr, i+1, last);
    }

    printf("%llu\r\n", last);

    return 0;
}