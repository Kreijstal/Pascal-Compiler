program fizzbuzz;
var
    i: integer;
begin
    for i := 1 to 100 do
    begin
        if (i mod 15) = 0 then
            write('FizzBuzz\n')
        else if (i mod 3) = 0 then
            write('Fizz\n')
        else if (i mod 5) = 0 then
            write('Buzz\n')
        else
        begin
            write(i);
            write('\n');
        end;
    end;
end.
