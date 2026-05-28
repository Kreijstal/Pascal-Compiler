{$mode objfpc}
{ Regression: `AnsiString(UnicodeStringExpr)` typecast must convert the
  UTF-16 source to ANSI bytes, not raw-copy the wide pointer.  Mirrors the
  FPC Win RTL `_FPC_ParamStrA` pattern: `Result:=AnsiString(ParamStrU(l))`.
  Without the fix the codegen routes the assignment through
  `kgpc_string_assign`, which duplicates the widestring via strlen() — the
  UTF-16 'h' is followed by a zero byte so the result truncates to "h"
  (1-char garbage), giving `Cannot open file "h"` when pp_win.exe is fed
  any multi-character argv[1] under wine. }
program tdd_widestring_to_ansistring_cast;
var
  ws: UnicodeString;
  s: AnsiString;
begin
  ws := 'hi.pas';
  s := AnsiString(ws);
  WriteLn('len=', Length(s));
  WriteLn('s=[', s, ']');
end.
