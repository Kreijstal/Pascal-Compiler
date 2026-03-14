#include <stdint.h>

/* Fallback converter used by FPC RTL generated helpers. */
char *widechar__op_assign_olevariant_wc(uint16_t value)
{
    static char table[256][2];
    static int inited = 0;
    static char fallback[2] = {'?', '\0'};
    if (!inited)
    {
        for (int i = 0; i < 256; ++i)
        {
            table[i][0] = (char)i;
            table[i][1] = '\0';
        }
        inited = 1;
    }
    if (value < 256)
        return table[value];
    return fallback;
}
