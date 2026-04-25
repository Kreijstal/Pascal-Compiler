program fpc_bootstrap_getmsgline_pchar_repro;
{$mode objfpc}

function GetMsgLine(var p: PChar): string;
var
  i: LongInt;
begin
  i := 0;
  while not (p^ in [#0, #10]) and (i < 256) do
  begin
    Inc(i);
    GetMsgLine[i] := p^;
    Inc(p);
  end;
  if p^ = #10 then
    Inc(p);
  if p^ = #0 then
    p := nil;
  GetMsgLine[0] := Chr(i);
end;

procedure DumpLines;
var
  p: PChar;
begin
  p :=
    '**2XLO_Define order of library linking'#10 +
    '**2XLL_Link using ld.lld GNU compatible LLVM linker'#10 +
    '**2Xm_Generate link map'#10;
  while Assigned(p) do
    WriteLn(GetMsgLine(p));
end;

begin
  DumpLines;
end.
