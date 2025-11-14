program typed_file_demo;

{ Simple runtime test for binary typed files using helper
  procedures assign/rewrite/reset/close and
  file_read_integer/file_write_integer. }

var
  F: file;
  i: Integer;

begin
  assign(F, 'tests/output/typed_file_demo_int.dat');
  rewrite(F);

  i := 1;
  file_write_integer(F, i);
  i := 2;
  file_write_integer(F, i);

  close(F);

  assign(F, 'tests/output/typed_file_demo_int.dat');
  reset(F);

  file_read_integer(F, i);
  writeln(i);
  file_read_integer(F, i);
  writeln(i);

  close(F);
end.
