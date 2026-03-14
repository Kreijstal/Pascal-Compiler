{ Test octstr function - converts integer to octal string }
program fpc_octstr;
{$mode objfpc}
begin
  WriteLn(octstr(65, 3));
  WriteLn(octstr(255, 3));
end.
