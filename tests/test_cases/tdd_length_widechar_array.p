{ Regression: under FPC's win64 RTL, `setup_arguments` declares
  `buf : array[0..MaxPathLen] of WideChar` then computes
  `nArg0W := GetModuleFileNameW(0, PWideChar(buf), Length(buf));`.
  Before the fix, KGPC's Length() codegen fallback only handled
  `Length(TypeName)`; a variable argument fell through to a generic
  call to `kgpc_shortstring_length`, which read buf[0] as a length
  byte and produced garbage.  GetModuleFileNameW was then called
  with cchFilePath=0, leaving argv pointing into the UTF-16 chars
  region and breaking command-line parsing -- pp_win.exe saw
  "hello.pas" as a single-byte "h" (the first byte of the wide
  string before its NUL terminator).

  This test exercises Length() on a fixed-size non-string array of
  WideChar / LongInt / Byte to confirm the value is computed at
  compile time as (high-low+1). }
{$mode objfpc}
program tdd_length_widechar_array;
var
  buf_w  : array[0..9] of WideChar;
  buf_l  : array[5..14] of LongInt;
  buf_b  : array[0..255] of Byte;
begin
  WriteLn('wide=', Length(buf_w));
  WriteLn('long=', Length(buf_l));
  WriteLn('byte=', Length(buf_b));
end.
