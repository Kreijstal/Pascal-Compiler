{ Test: Type helper bare Length property }
{ BUG: In type helpers, bare 'Length' should mean Length(Self) }
program type_helper_bare_length;

type
  TStringHelper = type helper for AnsiString
    function GetLen: Integer;
  end;

function TStringHelper.GetLen: Integer;
begin
  Result := Length;  // Should mean Length(Self)
end;

var
  S: AnsiString;
begin
  S := 'Hello';
  WriteLn(S.GetLen);
end.
