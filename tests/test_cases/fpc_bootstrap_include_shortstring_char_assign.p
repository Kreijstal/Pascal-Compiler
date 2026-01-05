program fpc_bootstrap_include_shortstring_char_assign;

{$mode objfpc}

{$I fpc_bootstrap_include_shortstring_char_assign.inc}

var
  S: TShort;

begin
  S := CharToShort('Z');
  writeln('len=', Length(S));
  writeln('ch=', S[1]);
end.
