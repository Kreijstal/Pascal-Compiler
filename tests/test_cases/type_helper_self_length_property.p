{ Test: Type helper Self.Length property access }
{ BUG: Self.Length should resolve to Length(Self) for string helpers }
program type_helper_self_length_property;

type
  TStringHelper = type helper for AnsiString
    function GetLast: Char;
  end;

function TStringHelper.GetLast: Char;
begin
  Result := Self[Self.Length];
end;

var
  S: AnsiString;
begin
  S := 'Hello';
  WriteLn(S.GetLast);
end.
