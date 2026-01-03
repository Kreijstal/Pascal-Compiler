{ Test: module property with write accessor (used in initc.pp for cerrno). }
program fpc_bootstrap_module_property_write;
{$mode objfpc}

var
  backing: LongInt;

function GetBacking: LongInt;
begin
  GetBacking := backing;
end;

procedure SetBacking(v: LongInt);
begin
  backing := v;
end;

property Value: LongInt read GetBacking write SetBacking;

begin
  Value := 42;
  WriteLn(Value);
end.
