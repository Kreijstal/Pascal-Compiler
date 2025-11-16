program RegisterSpillLimit;
type
    arr_t = array[0..15] of longint;
var
    data: arr_t;

procedure SumFirstEight(var x0, x1, x2, x3, x4, x5, x6, x7: longint);
begin
    x0 := x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7;
end;

var
    i: longint;
begin
    for i := 0 to 15 do
        data[i] := i + 1;
    SumFirstEight(data[0], data[1], data[2], data[3],
        data[4], data[5], data[6], data[7]);
    writeln(data[0]);
end.
