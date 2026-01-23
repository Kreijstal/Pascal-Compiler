{$mode objfpc}
{$H+}
program reg_overload_default_open_array;

function Foo(const A: array of Char; Count: Integer = 1): Integer; overload;
begin
  Result := Count + System.Length(A);
end;

function Foo(const A: array of Integer; Count: Integer = 2): Integer; overload;
begin
  Result := Count + System.Length(A);
end;

begin
  WriteLn(Foo(['a','b']));
end.
