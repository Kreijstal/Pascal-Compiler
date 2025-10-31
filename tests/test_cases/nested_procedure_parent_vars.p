program nested_procedure_test(output);
    var a, b, c, d: integer;

    function Add(x: integer; y: integer): integer;
    begin
        Add := x + y
    end;

    function Mult(x: integer; y: integer): integer;
    begin
        Mult := x * y
    end;

    procedure multadd;
    begin
        c := Add(a, b);
        d := Mult(c, a)
    end;

begin
    a := 5;
    b := 3;
    multadd;
    writeln(c);
    writeln(d)
end.
