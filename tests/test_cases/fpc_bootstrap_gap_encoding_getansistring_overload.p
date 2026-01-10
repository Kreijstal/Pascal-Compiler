program fpc_bootstrap_gap_encoding_getansistring_overload;

{$mode objfpc}

uses SysUtils;

var
  Bytes: TBytes;
  S: AnsiString;

begin
  SetLength(Bytes, 2);
  Bytes[0] := Ord('H');
  Bytes[1] := Ord('i');

  S := TEncoding.UTF8.GetAnsiString(Bytes, 0, Length(Bytes));
  writeln(S);
end.
