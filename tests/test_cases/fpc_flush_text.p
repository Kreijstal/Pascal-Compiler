{ Test Flush procedure for Text files }
program fpc_flush_text;
{$mode objfpc}
begin
  Write('hello');
  Flush(Output);
  WriteLn(' world');
end.
