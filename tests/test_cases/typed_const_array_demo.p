program typed_const_array_demo;

const
    Values: array[0..4] of integer = (1, 1, 2, 3, 5);

var
    i: integer;

begin
    for i := 0 to 4 do
        writeln(Values[i]);
end.
