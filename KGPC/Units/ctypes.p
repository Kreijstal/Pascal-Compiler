unit ctypes;

interface

type
    // Character types
    cschar = -128..127;
    cchar = -128..127;
    cuchar = 0..255;

    // Integer types
    cshort = -32768..32767;
    cushort = 0..65535;
    cint = -2147483648..2147483647;
    cuint = 0..4294967295;
    clong = -9223372036854775808..9223372036854775807;
    culong = 0..18446744073709551615;
    clonglong = -9223372036854775808..9223372036854775807;
    culonglong = 0..18446744073709551615;

    // Fixed-size aliases
    cint8 = -128..127;
    cuint8 = 0..255;
    cint16 = -32768..32767;
    cuint16 = 0..65535;
    cint32 = -2147483648..2147483647;
    cuint32 = 0..4294967295;
    cint64 = -9223372036854775808..9223372036854775807;
    cuint64 = 0..18446744073709551615;

    // Pointer-sized integer aliases
    csize_t = 0..18446744073709551615;
    cssize_t = -9223372036854775808..9223372036854775807;
    cptrdiff_t = -9223372036854775808..9223372036854775807;
    cintptr = -9223372036854775808..9223372036854775807;
    cuintptr = 0..18446744073709551615;
    cbool = 0..1;

    // Floating point types
    cfloat = real;
    cdouble = real;
    clongdouble = real;

    // Pointer helpers
    pcschar = ^cschar;
    pcchar = ^cchar;
    pcuchar = ^cuchar;
    pcshort = ^cshort;
    pcushort = ^cushort;
    pcint = ^cint;
    pcuint = ^cuint;
    pclong = ^clong;
    pculong = ^culong;
    pclonglong = ^clonglong;
    pculonglong = ^culonglong;
    pcint8 = ^cint8;
    pcuint8 = ^cuint8;
    pcint16 = ^cint16;
    pcuint16 = ^cuint16;
    pcint32 = ^cint32;
    pcuint32 = ^cuint32;
    pcint64 = ^cint64;
    pcuint64 = ^cuint64;
    pcsize_t = ^csize_t;
    pcssize_t = ^cssize_t;
    pcptrdiff_t = ^cptrdiff_t;
    pcintptr = ^cintptr;
    pcuintptr = ^cuintptr;
    pcbool = ^cbool;
    pcfloat = ^cfloat;
    pcdouble = ^cdouble;
    pclongdouble = ^clongdouble;

    // C-style strings
    cstring = string;

implementation

end.
