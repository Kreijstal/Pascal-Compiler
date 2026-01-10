unit type_helper_inheritance_unit;

interface

type
  TBaseHelper = type helper for AnsiString
  end;

  TStringHelper = type helper(TBaseHelper) for AnsiString
  end;

function AfterHelper: Integer;

implementation

function AfterHelper: Integer;
begin
  Result := 1;
end;

end.
