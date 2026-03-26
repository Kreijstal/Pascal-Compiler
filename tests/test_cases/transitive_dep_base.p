unit transitive_dep_base;
interface
type
  TGrandparent = class(TObject)
    FBaseVal: Integer;
    constructor Create;
  end;
implementation
constructor TGrandparent.Create;
begin
  FBaseVal := 10;
end;
end.
