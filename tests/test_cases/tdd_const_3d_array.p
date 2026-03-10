{ TDD: 3-dimensional const array (boundary case before 4D) }
program tdd_const_3d_array;

const
  Cube: array[0..1, 0..1, 0..2] of Integer = (
    ((1, 2, 3), (4, 5, 6)),
    ((7, 8, 9), (10, 11, 12))
  );

var
  X, Y, Z: Integer;
begin
  for X := 0 to 1 do
    for Y := 0 to 1 do
    begin
      for Z := 0 to 2 do
        Write(Cube[X, Y, Z], ' ');
      WriteLn;
    end;
end.
