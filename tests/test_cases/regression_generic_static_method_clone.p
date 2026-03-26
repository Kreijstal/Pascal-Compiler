{ Regression test: generic record with static class function
  Verifies that when a generic record is specialized, its static
  class functions are correctly cloned and callable, including
  methods that call other static methods on a type parameter.

  This pattern is used in heaptrc.pp (HashListTemplated<Control>
  with Control.KeyEqualsToItem calls).
}
{$mode objfpc}
program regression_generic_static_method_clone;

type
  TIntControl = record
    class function IsEqual(A, B: LongInt): Boolean; static;
  end;

  generic TContainer<TCtrl> = record
    Data: array[0..3] of LongInt;
    Count: LongInt;
    class function Find(var C: TContainer; Key: LongInt): LongInt; static;
  end;

class function TIntControl.IsEqual(A, B: LongInt): Boolean;
begin
  Result := (A = B);
end;

class function TContainer.Find(var C: TContainer; Key: LongInt): LongInt;
var
  I: LongInt;
begin
  Result := -1;
  for I := 0 to C.Count - 1 do
    if TCtrl.IsEqual(C.Data[I], Key) then
    begin
      Result := I;
      Exit;
    end;
end;

type
  TIntContainer = specialize TContainer<TIntControl>;

var
  C: TIntContainer;
  idx: LongInt;
begin
  C.Count := 3;
  C.Data[0] := 10;
  C.Data[1] := 20;
  C.Data[2] := 30;

  idx := TIntContainer.Find(C, 20);
  WriteLn('Find 20: ', idx);

  idx := TIntContainer.Find(C, 99);
  WriteLn('Find 99: ', idx);
end.
