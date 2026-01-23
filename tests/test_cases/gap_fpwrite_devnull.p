{$mode objfpc}
program gap_fpwrite_devnull;

uses
  BaseUnix;

var
  buf: array[0..1] of char;
  n: SizeInt;
  fd: cint;

begin
  fd := fpOpen('/dev/null', O_WRONLY);
  if fd = -1 then
  begin
    WriteLn('openfail');
    Halt(1);
  end;
  buf[0] := 'O';
  buf[1] := 'K';
  n := fpWrite(fd, buf, SizeOf(buf));
  fpClose(fd);
  WriteLn(n);
end.
