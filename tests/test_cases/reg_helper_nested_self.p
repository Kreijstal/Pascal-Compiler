{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
program reg_helper_nested_self;

type
  TStringHelper = type helper for AnsiString
    function FirstChar: Char;
  end;

function TStringHelper.FirstChar: Char;
  function Inner: Char;
  begin
    Result := Self[1];
  end;
begin
  Result := Inner;
end;

var
  S: AnsiString;
begin
  S := 'abc';
  WriteLn(S.FirstChar);
end.
