program OverloadArrayPointer;

{ Overload resolution for array vs pointer parameters should prefer the array overload. }

type
  TArr = array[0..1] of LongInt;
  PArr = ^TArr;

var
  a: TArr;
  called: LongInt;

procedure Foo(p: PArr); overload;
begin
  called := 1;
end;

procedure Foo(const p: TArr); overload;
begin
  called := 2;
end;

begin
  a[0] := 1;
  a[1] := 2;
  called := 0;
  Foo(a);
  if called = 2 then
    writeln('OK')
  else
    writeln('BAD');
end.
