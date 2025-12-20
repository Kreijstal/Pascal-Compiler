program dotted_alias_pointer;

uses dotted_alias_reexport_unit;

var
  x: LongInt;

begin
  x := 41;
  takes_pcint(@x);
  writeln(x);
end.
