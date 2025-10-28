program string_concat_demo;

var
    lhs, rhs, combined: string;
begin
    lhs := 'Hello';
    rhs := 'World';
    combined := lhs + ' ' + rhs;
    writeln(combined);
end.
