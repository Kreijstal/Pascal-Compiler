{$mode objfpc}
program tdd_const_reexport_string;

uses
  tdd_const_reexport_string_unit;

const
  LocalSuffix = tdd_const_reexport_string_unit.SharedSuffix;
  LocalGreeting = tdd_const_reexport_string_unit.Greeting;

begin
  WriteLn('suffix=', LocalSuffix);
  WriteLn('greeting=', LocalGreeting);
  WriteLn('combined=', LocalGreeting + LocalSuffix);
end.
