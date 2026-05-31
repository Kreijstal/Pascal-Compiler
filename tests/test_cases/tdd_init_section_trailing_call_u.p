{ Unit whose initialization section ends in a procedure CALL statement with
  no trailing semicolon, immediately followed by a finalization section whose
  body contains an assignment (':='). This reproduces the ccharset.pas
  `registermapping(@unicodemap)` shape from FPC's compiler. The parser's
  assignment-lookahead used to scan across the (>9 char) `finalization`
  keyword and latch onto the ':=' in the finalization body, mis-routing the
  final init call to the assignment parser and silently dropping it. }
unit tdd_init_section_trailing_call_u;
interface
var marker: integer;
procedure setm(v: integer);
implementation
procedure setm(v: integer); begin marker := v; end;
initialization
  marker := 1;
  setm(42)
finalization
  marker := 0
end.
