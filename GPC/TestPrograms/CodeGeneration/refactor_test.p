program refactor_test;
var
    i, j, k: integer;
begin
    i := 10;
    j := 20;
    k := 0;

    if i < j then
    begin
        writeln('i is less than j');
    end
    else
    begin
        writeln('i is not less than j');
    end;

    while k < 5 do
    begin
        write('k = ');
        writeln(k);
        k := k + 1;
    end;

    for i := 1 to 5 do
    begin
        write('i = ');
        write(i);
        write(', ');
    end;
    writeln('');
end.
