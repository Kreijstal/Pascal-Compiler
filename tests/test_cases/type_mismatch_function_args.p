{$mode objfpc}
program type_mismatch_function_args;

uses
  SysUtils;

function AddLengths(const A: string; const B: ShortString): Integer;
begin
  Result := Length(A) + Length(B);
end;

var
  S: ShortString;
  L: Integer;

begin
  S := 'hey';
  L := AddLengths('hi', S);
  writeln(L);
end.
