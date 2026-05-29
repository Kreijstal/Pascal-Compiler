{$mode objfpc}
program tdd_ansistr_to_widechararray;
uses sysutils;
{ Regression for KGPC issue: assigning an AnsiString into a fixed
  `array[..] of WideChar` was routed through `kgpc_string_to_shortstring`,
  producing a length-prefixed shortstring instead of a widechar payload.
  This broke FileRec.Name on the Win64 target — every reset()/rewrite()
  was passing CreateFileW a corrupt PWideChar.

  Expect each AnsiChar to be zero-extended to a UTF-16 unit; trailing
  elements past the source length are padded with zero. }
var
  warr: array[0..15] of widechar;
  s: ansistring;
  i: longint;
  ok: boolean;
const
  Expected: array[0..15] of word = (
    $0068, $0069, $002E, $0070, $0061, $0073,
    $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000);
begin
  s := 'hi.pas';
  warr := s;
  ok := true;
  for i := 0 to 15 do
    if Ord(warr[i]) <> Expected[i] then
      ok := false;
  if ok then
    WriteLn('OK')
  else begin
    Write('FAIL warr=');
    for i := 0 to 15 do
      Write(IntToHex(Ord(warr[i]), 4), ' ');
    WriteLn;
  end;
end.
