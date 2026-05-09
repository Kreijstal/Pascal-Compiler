program tdd_expr_tree_constant_simplify_dispatch;
begin
  writeln(2 + 3);
  writeln(8 - 3);
  writeln(2 * 3);

  if (4 < 9) then
    writeln('lt')
  else
    writeln('ge');

  if (4 >= 9) then
    writeln('bad')
  else
    writeln('ok');

  writeln(ord(#65));
  writeln(ord(true));
  writeln(ord(false));
end.
