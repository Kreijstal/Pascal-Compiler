{ Test that string[N] declarations use correct capacity (N+1) for storage,
  not the default 256. Assigning a string longer than N chars must be
  truncated to N and must NOT overflow into adjacent stack variables. }
program shortstring_sized_capacity;
{$mode objfpc}
{$H-}

var
  opt: string[32];
  helpLine: string;
begin
  helpLine := 'sentinel';
  opt := 'This is a very long string that exceeds thirty two characters easily';

  if Length(opt) > 32 then
  begin
    WriteLn('FAIL: opt length = ', Length(opt));
    Halt(1);
  end;

  if helpLine <> 'sentinel' then
  begin
    WriteLn('FAIL: helpLine corrupted to [', helpLine, ']');
    Halt(1);
  end;

  WriteLn('opt length: ', Length(opt));
  WriteLn('opt: ', opt);
  WriteLn('helpLine: ', helpLine);
end.
