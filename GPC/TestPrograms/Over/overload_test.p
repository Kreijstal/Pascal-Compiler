program overload_test;

var
    i1, i2, i3: integer;
var
    r1, r2, r3: real;

function add(a: integer; b: integer): integer;
begin
    add := a + b;
end;

function add(a: real; b: real): real;
begin
    add := a + b;
end;

function subtract(a: integer; b: integer): integer; cname
begin
    subtract := a - b;
end;

begin
    i1 := 5;
    i2 := 10;
    i3 := add(i1, i2);
    writeln(i3);

    r1 := 2.5;
    r2 := 3.5;
    r3 := add(r1, r2);
    writeln(r3);

    i3 := subtract(i2, i1);
    writeln(i3);
end.
