program reg_single_arg_spill;

function Sum10(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9: Single): Single;
begin
  Sum10 := a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
end;

var
  a0: Single;
  a1: Single;
  a2: Single;
  a3: Single;
  a4: Single;
  a5: Single;
  a6: Single;
  a7: Single;
  a8: Single;
  a9: Single;
  total: Single;

begin
  a0 := 1.25;
  a1 := 2.5;
  a2 := 3.75;
  a3 := 4.125;
  a4 := 5.5;
  a5 := 6.25;
  a6 := 7.75;
  a7 := 8.5;
  a8 := 9.25;
  a9 := 10.75;

  total := Sum10(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
  writeln(total:0:3);
end.
