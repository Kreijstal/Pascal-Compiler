{$mode objfpc}
program setcodepage_cp_utf8;

uses
  SysUtils;

var
  S: RawByteString;

begin
  S := 'hi';
  SetCodePage(S, CP_UTF8, False);
  writeln(Length(S));
end.
