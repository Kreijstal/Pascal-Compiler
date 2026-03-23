unit cross_unit_inherit_child;
interface
uses cross_unit_inherit_base;
type
  TChildItem = class(TBaseItem)
    FChildData: Integer;
    constructor Create;
  end;
implementation
constructor TChildItem.Create;
begin
  inherited Create;
  FChildData := 99;
end;
end.
