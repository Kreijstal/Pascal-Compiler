unit impl_type_after_method_unit;

interface

type
  TGUID = record
  end;

  TGUIDHelper = record helper for TGUID
    class function Create: TGUID; static;
    function ToString: string;
  end;

implementation

function TGUIDHelper.ToString: string;
begin
  Result := '';
end;

type
  TTrimMode = (Left, Right, Both);

function AfterMethod: Integer;
begin
  Result := 0;
end;

end.
