(* ERROR: objects of different types appear in the same expression *)
program main( input, output );
  var
    a: integer;
    x: real;
begin
  a := a + 1.23;
  x := x + 123
end.

