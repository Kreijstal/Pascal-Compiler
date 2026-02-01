{ TDD test for DoCapSizeInt with widestringmanager pattern
  This test verifies that:
  1. Procedural field types with return types are properly resolved
  2. DoCapSizeInt overload matches when the argument comes from a 
     function pointer call in a record field
  3. Assignment to procedural fields works correctly
  
  Verified: Passes with FPC and KGPC
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
