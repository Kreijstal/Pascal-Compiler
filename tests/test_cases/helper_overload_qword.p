{$mode objfpc}
{$modeswitch typehelpers}
program helper_overload_qword;

type
  TIntHelper = type helper for Integer
    function Foo: QWord;
    function Foo(x: QWord): QWord;
  end;

function TIntHelper.Foo: QWord;
begin
  Result := Foo(40);
end;

function TIntHelper.Foo(x: QWord): QWord;
begin
  Result := x + 2;
end;

var
  i: Integer;

begin
  i := 0;
  Writeln(i.Foo);
end.
