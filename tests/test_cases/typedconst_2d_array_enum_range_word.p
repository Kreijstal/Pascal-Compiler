{ Regression: 2-D typed-const array indexed by an enum subrange with non-zero
  low bound, with 2-byte element type. The widechar size-2 heuristic in
  codegen_array_element_address used to mis-flag the outer dimension as
  a WideChar index and override the row stride to 2 (=elem_size) instead of
  row_count*elem_size. The typed-const initialiser then wrote each element
  ignoring row stride, collapsing the table. This is the same shape as
  FPC's compiler/x86/cgx86.pas convertopsse:
    array[OS_F32..OS_F128, OS_F32..OS_F128] of tasmop
  whose miscompilation caused pp_bootstrap to crash with
  internalerror(200312205) the first time it lowered an assignment to a
  Double field. }
program typedconst_2d_array_enum_range_word;
{$mode objfpc}
type
  tsize = (S_NO, S_8, S_16, S_32, S_64, S_128, S_S8, S_S16, S_S32, S_S64, S_S128,
           S_F32, S_F64, S_F80, S_C64, S_F128);
const
  tbl : array[S_F32..S_F128, S_F32..S_F128] of word = (
    (1, 2, 0, 0, 0),
    (2, 1, 0, 0, 0),
    (0, 0, 0, 0, 0),
    (0, 0, 0, 1, 0),
    (0, 0, 0, 0, 3));
var
  s, t: tsize;
begin
  s := S_F64;
  t := S_F64;
  writeln('[F32,F32]=', tbl[S_F32, S_F32]);
  writeln('[F64,F64]=', tbl[s, t]);
  writeln('[F32,F64]=', tbl[S_F32, S_F64]);
  writeln('[F64,F32]=', tbl[S_F64, S_F32]);
  writeln('[C64,C64]=', tbl[S_C64, S_C64]);
  writeln('[F128,F128]=', tbl[S_F128, S_F128]);
  writeln('[F32,F128]=', tbl[S_F32, S_F128]);
  writeln('[F128,F32]=', tbl[S_F128, S_F32]);
end.
