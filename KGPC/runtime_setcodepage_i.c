#include <stdint.h>

void setcodepage_rbs_i_b(void *s_arg, int32_t codepage, int32_t convert);

void setcodepage_rbs_i(void *s_arg, int32_t codepage)
{
    setcodepage_rbs_i_b(s_arg, codepage, 1);
}
