program deeply_nested_test(output);
var
    global_var: integer;

procedure outer;
var
    outer_local: integer;

    procedure mid;
    var
        mid_local: integer;

        procedure inner;
        var
            inner_local: integer;
        begin
            inner_local := 10;
            outer_local := outer_local + inner_local;
            global_var := global_var + outer_local + mid_local + inner_local;
            writeln(outer_local);
            writeln(global_var);
        end;

    begin
        mid_local := 3;
        inner;
        writeln(outer_local);
    end;

begin
    outer_local := 5;
    mid;
end;

begin
    global_var := 1;
    outer;
    writeln(global_var);
end.
