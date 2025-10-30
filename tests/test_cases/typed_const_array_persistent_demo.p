program typed_const_array_persistent_demo;

procedure EmitAndIncrement;
const
    Values: array[0..2] of integer = (10, 20, 30);
var
    i: integer;
begin
    for i := 0 to 2 do
    begin
        writeln(Values[i]);
        Values[i] := Values[i] + 1;
    end;
end;

begin
    EmitAndIncrement;
    EmitAndIncrement;
end.
