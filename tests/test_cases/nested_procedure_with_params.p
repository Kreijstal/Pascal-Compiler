program nested_procedure_with_params(input, output);
var
    base, total: integer;

procedure outer(x: integer; y: integer);
var
    increment: integer;

    procedure inner(z: integer; w: integer);
    begin
        total := total + x + y + z + w + base;
        increment := increment + z + w;
    end;

begin
    increment := 2;
    total := x;
    inner(y, 3);
    writeln(total);
    writeln(increment);
end;

begin
    base := 4;
    total := 0;
    outer(5, 6);
end.
