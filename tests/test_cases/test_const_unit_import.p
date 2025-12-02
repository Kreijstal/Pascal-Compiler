program test_const_unit_import;
uses const_unit_comment;
begin
  writeln('FileMode: ', FileMode);
  if IsMultiThread then
    writeln('IsMultiThread: TRUE')
  else
    writeln('IsMultiThread: FALSE');
  ExitCode := 123;
  writeln('ExitCode: ', ExitCode);
end.
