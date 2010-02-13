#include <stdio.h>

#include <sys/types.h>
typedef uint32_t uint32;
typedef uint8_t uint8;

typedef struct
    {
    void (*put)(void *info, uint8 ch);
    void *info;
    } stupid_ostream;

void test(stupid_ostream *out);

void put(void *info, uint8 ch)
    {
    putchar(ch);
    }

int main(int argc, char **argv)
    {
    stupid_ostream t;

    t.put = put;
    test(&t);

    return 0;
    }
