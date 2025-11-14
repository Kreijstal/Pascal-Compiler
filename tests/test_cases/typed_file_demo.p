program typed_file_demo;

{ Simple runtime test for binary typed files using helper
  procedures file_assign/file_rewrite/file_reset/file_close and
  file_read_integer/file_write_integer. }

var
  F: file;
  i: Integer;

begin
  file_assign(F, 'tests/output/typed_file_demo_int.dat');
  file_rewrite(F);

  i := 1;
  file_write_integer(F, i);
  i := 2;
  file_write_integer(F, i);

  file_close(F);

  file_assign(F, 'tests/output/typed_file_demo_int.dat');
  file_reset(F);

  file_read_integer(F, i);
  writeln(i);
  file_read_integer(F, i);
  writeln(i);

  file_close(F);
end.

