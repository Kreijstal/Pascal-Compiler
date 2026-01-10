program fpc_bootstrap_gap_stringtoguid;

{$mode objfpc}

uses SysUtils;

var
  G: TGUID;

begin
  G := StringToGUID('{00112233-4455-6677-8899-AABBCCDDEEFF}');
  writeln(G.D1);
end.
