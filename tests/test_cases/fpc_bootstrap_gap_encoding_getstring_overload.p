program fpc_bootstrap_gap_encoding_getstring_overload;

{$mode objfpc}

uses SysUtils;

var
  Bytes: TBytes;
  S: UnicodeString;

begin
  SetLength(Bytes, 3);
  Bytes[0] := Ord('A');
  Bytes[1] := Ord('B');
  Bytes[2] := Ord('C');

  S := TEncoding.UTF8.GetString(Bytes, 0, Length(Bytes));
  writeln(S);
end.
