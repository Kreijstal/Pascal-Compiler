unit transitive_dep_middle;
interface
uses transitive_dep_base;
type
  TParent = class(TGrandparent)
    FMiddleVal: Integer;
    constructor Create;
  end;
implementation
constructor TParent.Create;
begin
  inherited Create;
  FMiddleVal := 20;
end;
end.
