{ TDD test for DoCapSizeInt with widestringmanager pattern
  This test exposes a bug where:
  1. Procedural field types from FPC's System unit aren't properly resolved
  2. DoCapSizeInt overload can't match when the argument comes from a 
     function pointer call in a record field
  
  Verified: Passes with FPC, fails with KGPC when using --no-stdlib
}
{$mode objfpc}
program tdd_capsizeint_wsm;

type
  TCompareFunc = function(const S1, S2: AnsiString): SizeInt;

  TWideStringManager = record
    CompareStrAnsiStringProc: TCompareFunc;
  end;

var
  widestringmanager: TWideStringManager;

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

function AnsiCompareStr(const S1, S2: AnsiString): Integer;
begin
  Result := DoCapSizeInt(widestringmanager.CompareStrAnsiStringProc(S1, S2));
end;

begin
  widestringmanager.CompareStrAnsiStringProc := @MyCompare;
  WriteLn(AnsiCompareStr('hello', 'hi'));
end.
