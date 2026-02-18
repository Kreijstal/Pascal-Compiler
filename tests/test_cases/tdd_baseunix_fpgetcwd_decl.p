program tdd_baseunix_fpgetcwd_decl;
{$mode objfpc}
uses baseunix;

var
  buf: array[0..511] of Char;
  p: PChar;
begin
  p := fpgetcwd(@buf[0], SizeOf(buf));
  if p <> nil then
    writeln('ok')
  else
    writeln('nil');
end.
