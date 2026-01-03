program fpc_bootstrap_string_n_length;
{$mode objfpc}
{ Bootstrap blocker: string[N] should truncate to N (e.g., TLineEndStr = string[3]) }

type
  TLineEndStr = string[3];

var
  s: TLineEndStr;

begin
  s := 'abcdef';
  writeln(Length(s));
  writeln(s);
end.
