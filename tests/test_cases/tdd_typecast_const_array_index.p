{ Regression test for issue #537: typed const array with typecast-wrapped
  constant indices uses 0-based init instead of actual constant values. }
program tdd_typecast_const_array_index;
type
  tregister = type LongWord;
const
  NR_ES = tregister($05000000);
  NR_CS = tregister($05000001);
  NR_SS = tregister($05000002);
  NR_DS = tregister($05000003);
  NR_FS = tregister($05000004);
  NR_GS = tregister($05000005);
const
  segprefixes: array[NR_ES..NR_GS] of Byte = ($26, $2E, $36, $3E, $64, $65);
begin
  WriteLn(segprefixes[NR_ES]);
  WriteLn(segprefixes[NR_GS]);
end.
