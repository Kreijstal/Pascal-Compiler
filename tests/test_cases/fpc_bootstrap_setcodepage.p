program fpc_bootstrap_setcodepage;

{$mode objfpc}

uses SysUtils;

var
  S: RawByteString;

begin
  S := 'abc';
  SetCodePage(S, 65001, False);
  writeln('cp=', StringCodePage(S));
end.
