unit cross_unit_class_chain_base;
interface
type
  TInner = class(TObject)
    FLevel: Integer;
    constructor Create;
  end;
implementation
constructor TInner.Create;
begin
  inherited Create;
  FLevel := 42;
end;
end.
