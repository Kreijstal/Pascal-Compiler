program tdd_static_class_procvar_unqualified_in_method;
{$mode objfpc}

type
  TCmpFn = function(x: LongInt): LongInt;

  TFoo = class
    class function Doubler(x: LongInt): LongInt; static;
    function Dispatch(comparer: TCmpFn): LongInt;
    function RunTest: LongInt;
  end;

class function TFoo.Doubler(x: LongInt): LongInt;
begin
  Result := x * 2;
end;

function TFoo.Dispatch(comparer: TCmpFn): LongInt;
begin
  Result := comparer(21);
end;

function TFoo.RunTest: LongInt;
begin
  Result := Dispatch(@Doubler);
end;

var
  f: TFoo;
begin
  f := TFoo.Create;
  WriteLn(f.RunTest);
  f.Free;
end.
