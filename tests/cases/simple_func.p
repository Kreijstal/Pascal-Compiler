program SimpleFunc;
var
  x: integer;

function get_five: integer;
begin
  get_five := 5;
end;

begin
  x := get_five;
  writeln(x);
end.
