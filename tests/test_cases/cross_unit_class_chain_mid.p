unit cross_unit_class_chain_mid;
interface
uses cross_unit_class_chain_base;
type
  TProcInfo = class(TObject)
    inner: TInner;
    constructor Create;
  end;
var
  current: TProcInfo;
implementation
constructor TProcInfo.Create;
begin
  inherited Create;
  inner := TInner.Create;
end;
end.
