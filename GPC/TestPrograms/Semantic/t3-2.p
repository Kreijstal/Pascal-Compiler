(* ERROR: test expressions in IF/WHILE statements must be Boolean *)
program main( input, output );
  var a: integer;
begin
  while (a + 123) > 0 do
    a := a + 1
end.

