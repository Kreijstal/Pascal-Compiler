program unit_nested_record_access;

{$mode objfpc}

uses unit_nested_record;

begin
  unit_nested_record.Outer.Inner.Value := 7;
  writeln(unit_nested_record.Outer.Inner.Value);
end.
