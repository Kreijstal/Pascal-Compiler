program sign(input, output);

function signum(x: integer): integer;
begin
        if x > 0 then
                signum := 1
        else if x < 0 then
                signum := -1
        else
                signum := 0;
end;

var
        value: integer;

begin
        read(value);
        writeln(signum(value));
end.
