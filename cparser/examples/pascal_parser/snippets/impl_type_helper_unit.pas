unit impl_type_helper_unit;

interface

implementation

type
  TBaseHelper = type helper for AnsiString
  end;

  TStringHelper = type helper(TBaseHelper) for AnsiString
  end;

function AfterHelper: Integer;
begin
  Result := 1;
end;

end.
