{$mode objfpc}
program reg_sysutils_fpread;

uses BaseUnix;

var
  fd: cint;
  buf: array[0..3] of char;
  r: ssize_t;
begin
  fd := fpOpen('/dev/null', O_RDONLY);
  r := fpRead(fd, buf, size_t(SizeOf(buf)));
  if r >= 0 then
    Writeln('ok')
  else
    Writeln('err');
  fpClose(fd);
end.
