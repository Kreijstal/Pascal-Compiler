{ TDD: inline procedural fields in records should preserve return types }
{$mode objfpc}
program tdd_record_inline_funcptr;

type
  TManager = record
    CompareProc: function(const S1, S2: AnsiString): SizeInt;
  end;

var
  M: TManager;

function MyCompare(const S1, S2: AnsiString): SizeInt;
begin
  Result := Length(S1) - Length(S2);
end;

function DoCapSizeInt(SI: SizeInt): Integer; inline;
begin
  if SI < 0 then
    Result := -1
  else if SI > 0 then
    Result := 1
  else
    Result := 0;
end;

begin
  M.CompareProc := @MyCompare;
  WriteLn(DoCapSizeInt(M.CompareProc('hello', 'hi')));
end.
