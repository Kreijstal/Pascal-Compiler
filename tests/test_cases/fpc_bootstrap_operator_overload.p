{
  Test case: Operator overloading syntax not supported.

  BUG: KGPC parser doesn't recognize 'operator' keyword for defining
  custom operators on record types.
  Error: "Unexpected content before final '.'"

  This blocks FPC bootstrap because constexp.pas and many other units use:
    operator + (const a, b: Tconstexprint): Tconstexprint;
    operator := (const c: Tconstexprint): qword;
    etc.

  FPC correctly handles operator overloading.
}
program fpc_bootstrap_operator_overload;
{$mode objfpc}

type
  TMyInt = record
    value: Integer;
  end;

operator + (const a, b: TMyInt): TMyInt;
begin
  Result.value := a.value + b.value;
end;

var
  x, y, z: TMyInt;
begin
  x.value := 10;
  y.value := 20;
  z := x + y;
  WriteLn(z.value);
end.
