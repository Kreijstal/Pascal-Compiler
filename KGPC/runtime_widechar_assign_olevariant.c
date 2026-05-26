#include <stdint.h>

/* Wide-char-to-PAnsiChar converter used by FPC RTL generated helpers.
 * Returns a stub '?' byte string for code points >= 256. */
char *widechar__op_assign_olevariant_wc(uint16_t value) {
  static char table[256][2];
  static int inited = 0;
  static char stub_widechar[2] = {'?', '\0'};
  if (!inited) {
    for (int i = 0; i < 256; ++i) {
      table[i][0] = (char)i;
      table[i][1] = '\0';
    }
    inited = 1;
  }
  if (value < 256)
    return table[value];
  return stub_widechar;
}
