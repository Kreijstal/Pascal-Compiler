program mod_test;
var
    i: integer;
    mod3: integer;
begin
    for i := 0 to 9 do
    begin
        mod3 := i mod 3;
        write(i);
        write(' mod 3 = ');
        writeln(mod3);
    end;
end.
