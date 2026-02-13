program tdd_openstring_pchar_to_shortstr;

{$mode objfpc}{$H+}

procedure fpc_pchar_to_shortstr(var res: openstring; p: PAnsiChar);
  external name 'FPC_PCHAR_TO_SHORTSTR';

function ConvertAndChecksum(const Prefix: string; p: PAnsiChar;
  out OutShort: ShortString; out Sum: Integer): string;
var
  Temp: string;
  Index: Integer;
begin
  fpc_pchar_to_shortstr(OutShort, p);
  Temp := Prefix;
  fpc_pchar_to_shortstr(Temp, p);
  Result := Temp + '|' + OutShort;
  for Index := 1 to Length(OutShort) do
    Inc(Sum, Ord(OutShort[Index]));
end;

var
  Buffer: array[0..31] of Char;
  Cursor: PAnsiChar;
  Value: string;
  ShortValue: ShortString;
  Total: Integer;
  I: Integer;
begin
  for I := 0 to High(Buffer) do
    Buffer[I] := #0;

  for I := 0 to 10 do
    Buffer[I] := Chr(Ord('A') + I);
  Buffer[11] := #0;

  Cursor := @Buffer[0];
  Total := 0;
  Value := ConvertAndChecksum('prefix-', Cursor, ShortValue, Total);
  Writeln('s=', Value);
  Writeln('short=', ShortValue);
  Writeln('len=', Length(ShortValue));
  Writeln('sum=', Total);
end.
