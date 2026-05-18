program fpc_bootstrap_copy_shortstring_dollar_id;
{$mode objfpc}

type
  TIDString = string[127];

var
  s, t: TIDString;

begin
  s := '$rtti_header$13';
  writeln('s=', s, ' len=', length(s));
  t := Copy(s, 2, length(s));
  writeln('t="', t, '" len=', length(t));
end.
