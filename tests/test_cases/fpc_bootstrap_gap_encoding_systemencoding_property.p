program fpc_bootstrap_gap_encoding_systemencoding_property;

{$mode objfpc}

uses SysUtils;

var
  Enc: TEncoding;

begin
  Enc := TEncoding.SystemEncoding;
  if Enc <> nil then
    writeln('ok');
end.
