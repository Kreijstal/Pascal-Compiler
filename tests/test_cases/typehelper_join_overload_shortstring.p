program typehelper_join_overload_shortstring;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TStringType = ShortString;

  TStringHelper = type helper for TStringType
    class function Join(const Sep: TStringType; const Values: array of const): TStringType; overload; static;
    class function Join(const Sep: TStringType; const Values: array of TStringType): TStringType; overload; static;
  end;

class function TStringHelper.Join(const Sep: TStringType; const Values: array of const): TStringType;
var
  SValues: array of TStringType;
begin
  SetLength(SValues, Length(Values));
  if Length(SValues) > 0 then
    SValues[0] := 'aa';
  Result := Join(Sep, SValues);
end;

class function TStringHelper.Join(const Sep: TStringType; const Values: array of TStringType): TStringType;
begin
  if Length(Values) = 0 then
    Result := ''
  else
    Result := Values[0] + Sep;
end;

var
  s: TStringType;
  sep: TStringType;

begin
  sep := ', ';
  s := TStringType.Join(sep, ['x']);
  writeln(s);
end.
