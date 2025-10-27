program array_const;
const
    first = 1;
    last = 4;
    base = 5;
var
    arr: array[1..4] of integer;
    i: integer;
begin
    arr[1] := base;
    arr[2] := base + 1;
    arr[3] := base + 2;
    arr[4] := base + 3;
    for i := first to last do
    begin
        writeln(arr[i]);
    end;
end.
