program expr_stmt_relop;
{$mode objfpc}

var
  x: longint;
  y: longint;

begin
  x := 5;
  y := 5;
  (x = y);
  writeln('ok');
end.
