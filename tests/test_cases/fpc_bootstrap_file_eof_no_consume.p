program fpc_bootstrap_file_eof_no_consume;
{
  Regression test: eof() on an untyped File (record size 1) must
  not consume / advance the kernel position via stdio read-ahead.

  This mirrors what FPC's TCFileStream does internally:
    system.assign + system.reset(F, 1)  -> opens via syscall, stores fd
    system.blockread / system.eof on F  -> raw read() syscalls

  Before the fix, eof(F) used fgetc(stream) on a dup'd FILE*, which filled
  stdio's 8KB buffer from the kernel fd and silently skipped 8KB of data
  for the subsequent blockread.  When the file was smaller than 8KB, eof
  also misfired because the ungetc'd byte kept fgetc returning a value
  forever, producing an infinite loop in the FPC scanner.

  This test creates a file of 32 bytes, calls eof() repeatedly before
  reading anything, then reads all bytes via blockread, and checks that
  the byte sequence is intact.
}
{$mode objfpc}

const
  FNAME  = 'fpc_bootstrap_file_eof_no_consume.tmp';
  NBYTES = 32;

var
  f       : file;
  buf     : array[0..63] of byte;
  written : longint;
  total   : longint;
  i       : integer;
  ok      : boolean;
  pre_eof : boolean;
  post_eof: boolean;

begin
  { Create the file with a known byte pattern. }
  assign(f, FNAME);
  rewrite(f, 1);
  for i := 0 to NBYTES - 1 do
    buf[i] := byte((i * 7 + 3) and $FF);
  blockwrite(f, buf, NBYTES, written);
  close(f);
  if written <> NBYTES then
  begin
    writeln('FAIL: blockwrite wrote ', written, ' instead of ', NBYTES);
    halt(1);
  end;

  { Read it back the same way TCFileStream does: untyped File, record
    size 1, plain reset.  Call eof() BEFORE reading anything — this is
    the path that used to silently advance the kernel position. }
  assign(f, FNAME);
  reset(f, 1);

  pre_eof := eof(f);
  if pre_eof then
  begin
    writeln('FAIL: eof was true before any read on a non-empty file');
    close(f);
    halt(1);
  end;

  { Call eof() a few more times — none of these must consume bytes. }
  for i := 1 to 5 do
  begin
    if eof(f) then
    begin
      writeln('FAIL: eof returned true after ', i, ' pre-read calls');
      close(f);
      halt(1);
    end;
  end;

  { Now drain via blockread in tiny chunks and verify the bytes. }
  for i := 0 to 63 do
    buf[i] := 0;
  total := 0;
  ok := true;
  while not eof(f) do
  begin
    blockread(f, buf[total], 1, written);
    if written <> 1 then
      break;
    if buf[total] <> byte((total * 7 + 3) and $FF) then
    begin
      writeln('FAIL: byte ', total, ' = ', buf[total],
              ' expected ', byte((total * 7 + 3) and $FF));
      ok := false;
    end;
    total := total + written;
    if total >= NBYTES then
      break;
  end;

  post_eof := eof(f);
  close(f);

  if total <> NBYTES then
  begin
    writeln('FAIL: read ', total, ' bytes, expected ', NBYTES);
    halt(1);
  end;
  if not post_eof then
  begin
    writeln('FAIL: eof was false after reading all ', NBYTES, ' bytes');
    halt(1);
  end;
  if not ok then
    halt(1);

  writeln('OK total=', total);
end.
