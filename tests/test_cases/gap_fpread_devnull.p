{$mode objfpc}
program gap_fpread_devnull;

uses
  BaseUnix;

var
  buf: array[0..3] of byte;
  n: SizeInt;
  fd: cint;

begin
  fd := fpOpen('/dev/null', O_RDONLY);
  if fd = -1 then
  begin
    WriteLn('openfail');
    Halt(1);
  end;
  n := fpRead(fd, buf, SizeOf(buf));
  fpClose(fd);
  WriteLn(n);
end.
