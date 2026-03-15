program typed_file_demo;

{ Simple runtime test for binary typed files using
  BlockWrite/BlockRead with untyped file. }

var
  F: file;
  i: Integer;
  count: LongInt;

begin
  assign(F, 'tests/output/typed_file_demo_int.dat');
  rewrite(F, 1);

  i := 1;
  BlockWrite(F, i, SizeOf(i));
  i := 2;
  BlockWrite(F, i, SizeOf(i));

  close(F);

  assign(F, 'tests/output/typed_file_demo_int.dat');
  reset(F, 1);

  BlockRead(F, i, SizeOf(i), count);
  writeln(i);
  BlockRead(F, i, SizeOf(i), count);
  writeln(i);

  close(F);
end.
