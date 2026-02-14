program test_fpc_bootstrap_proctype_cast_call;
{$mode objfpc}

{ Test procedural type typecast followed by call: TypeName(ptr)()
  This pattern is used in FPC's system unit for VMT dispatch:
    TClassGetter = function: TObject of object;
    TObject(obj) := TClassGetter(Getter)();
  The parser must convert FUNC_CALL(TYPECAST(TypeName, source), outer_args)
  into a procedural variable call through the typecast result. }

type
  TIntFunc = function: Integer;
  TStrFunc = function(n: Integer): String;
  TAddFunc = function(a, b: Integer): Integer;

function GetFortyTwo: Integer;
begin
  Result := 42;
end;

function FormatNum(n: Integer): String;
begin
  if n < 0 then
    Result := 'negative'
  else if n = 0 then
    Result := 'zero'
  else
    Result := 'positive';
end;

function AddNums(a, b: Integer): Integer;
begin
  Result := a + b;
end;

{ Test calling through a Pointer variable typecast }
procedure TestPointerCast;
var
  p: Pointer;
  v: Integer;
begin
  p := @GetFortyTwo;
  v := TIntFunc(p)();
  WriteLn(v);
end;

{ Test calling through a function typecast with arguments }
procedure TestWithArgs;
var
  p: Pointer;
  s: String;
begin
  p := @FormatNum;
  s := TStrFunc(p)(42);
  WriteLn(s);
  s := TStrFunc(p)(-1);
  WriteLn(s);
  s := TStrFunc(p)(0);
  WriteLn(s);
end;

{ Test calling through a function typecast with multiple args }
procedure TestMultiArg;
var
  p: Pointer;
  v: Integer;
begin
  p := @AddNums;
  v := TAddFunc(p)(10, 32);
  WriteLn(v);
end;

begin
  TestPointerCast;
  TestWithArgs;
  TestMultiArg;
end.
