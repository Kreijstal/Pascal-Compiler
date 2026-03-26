unit cross_unit_inherit_base;
interface
type
  TBaseItem = class
    FBaseData: Integer;
    constructor Create;
  end;
implementation
constructor TBaseItem.Create;
begin
  FBaseData := 42;
end;
end.
