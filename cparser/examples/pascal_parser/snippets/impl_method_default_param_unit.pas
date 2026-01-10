unit impl_method_default_param_unit;

interface

type
  TEndian = (Little, Big);
  TBytes = array of Byte;
  TGUID = record
  end;

  TGUIDHelper = record helper for TGUID
    function ToByteArray(DataEndian: TEndian = Little): TBytes;
  end;

implementation

function TGUIDHelper.ToByteArray(DataEndian: TEndian = Little): TBytes;
begin
end;

function AfterMethod: Integer;
begin
  Result := 0;
end;

end.
