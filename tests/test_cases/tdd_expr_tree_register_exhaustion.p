program tdd_expr_tree_register_exhaustion;
var
    a, b, c, d, e, f, g, h: longint;
    i, j, k, l, m, n, o, p: longint;
    total: longint;
begin
    a := 1;  b := 2;  c := 3;  d := 4;
    e := 5;  f := 6;  g := 7;  h := 8;
    i := 9;  j := 10; k := 11; l := 12;
    m := 13; n := 14; o := 15; p := 16;

    total := ((a + b) * (c + d)) +
             ((e + f) * (g + h)) +
             ((i + j) * (k + l)) +
             ((m + n) * (o + p));

    writeln(total);
end.
