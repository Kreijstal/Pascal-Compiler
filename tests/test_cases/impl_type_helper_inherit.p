{$mode objfpc}
{$modeswitch typehelpers}
{ Test: Type helper inheritance in implementation }
{ Verifies parser handles type helper(ParentHelper) syntax }
program impl_type_helper_inherit;

type
  TBaseHelper = type helper for AnsiString
    function GetBase: Integer;
  end;

  TStringHelper = type helper(TBaseHelper) for AnsiString
    function GetDerived: Integer;
  end;

function TBaseHelper.GetBase: Integer;
begin
  Result := 10;
end;

function TStringHelper.GetDerived: Integer;
begin
  Result := GetBase + 5;
end;

var
  S: AnsiString;
begin
  S := 'Hello';
  WriteLn(S.GetDerived);
end.
