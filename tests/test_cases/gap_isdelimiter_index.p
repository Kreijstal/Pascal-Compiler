{$mode objfpc}
program gap_isdelimiter_index;

uses
  SysUtils;

begin
  if IsDelimiter(' ,', 'a,b', 2) then
    WriteLn('yes')
  else
    WriteLn('no');
end.
