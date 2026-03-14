{ Test PUint16 pointer type - required for FPC compiler units }
program fpc_puint16_typecast;

{$mode objfpc}

var
  val: UInt16;
  p: PUint16;
begin
  val := 12345;
  p := @val;
  WriteLn(p^);
end.
