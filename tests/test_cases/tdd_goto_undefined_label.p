program tdd_goto_undefined_label;
label 1;
begin
  goto 2;
  1: writeln('reachable');
end.
