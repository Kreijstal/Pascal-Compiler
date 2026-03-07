#include <stdint.h>

/* Portable fallback used when compiler-emitted SetCodePage helpers are absent.
 * Keep behavior conservative: metadata conversion is a no-op. */
void setcodepage_rbs_i_b(void *s_arg, int32_t codepage, int32_t convert)
{
    (void)s_arg;
    (void)codepage;
    (void)convert;
}
