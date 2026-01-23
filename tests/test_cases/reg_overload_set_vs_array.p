{$mode objfpc}
{$H+}
program reg_overload_set_vs_array;

type
  TCharSet = set of Char;

function Foo(const A: array of Char): Integer; overload;
begin
  Result := System.Length(A);
end;

function Foo(const A: TCharSet): Integer; overload;
begin
  Result := 99;
end;

begin
  WriteLn(Foo(['a','b']));
end.
