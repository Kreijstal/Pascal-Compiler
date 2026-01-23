{$mode objfpc}
{$H+}
program reg_overload_open_array_strings;

function Foo(const A: array of AnsiString; StartIndex: SizeInt = 0): SizeInt; overload;
begin
  Result := System.Length(A) + StartIndex;
end;

function Foo(const A: array of Char; StartIndex: SizeInt = 0): SizeInt; overload;
begin
  Result := -1;
end;

begin
  WriteLn(Foo(['ab','cd']));
end.
