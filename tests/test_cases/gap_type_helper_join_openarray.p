program gap_type_helper_join_openarray;
{$mode objfpc}
{$modeswitch typehelpers}

type
  TAnsiStringHelper = type helper for AnsiString
    class function Join(const Separator: AnsiString; const Values: array of AnsiString;
      StartIndex: SizeInt; ACount: SizeInt): AnsiString; static;
  end;

class function TAnsiStringHelper.Join(const Separator: AnsiString;
  const Values: array of AnsiString; StartIndex: SizeInt; ACount: SizeInt): AnsiString;
var
  VLen, I, CountLim, NR, NSep, N: SizeInt;
  Rp: PAnsiChar;
begin
  VLen := System.Length(Values);
  CountLim := VLen - StartIndex;
  if ACount > CountLim then
    ACount := CountLim;
  if ACount <= 0 then
    Exit('');
  NSep := System.Length(Separator);
  NR := (ACount - 1) * NSep;
  for I := StartIndex to StartIndex + ACount - 1 do
    NR := NR + System.Length(Values[I]);
  SetLength(Result, NR);
  Rp := @Result[1];
  N := System.Length(Values[StartIndex]);
  Move(Values[StartIndex][1], Rp^, N * SizeOf(AnsiChar));
end;

begin
end.
