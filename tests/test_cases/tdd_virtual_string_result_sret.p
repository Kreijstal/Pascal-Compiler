program TddVirtualStringResultSret;
{$mode objfpc}
{$H-}

type
  TBase = class
    Marker: LongInt;
    function OwnerHierarchyName: string; virtual;
  end;

  TChild = class(TBase)
    function OwnerHierarchyName: string; override;
  end;

function TBase.OwnerHierarchyName: string;
begin
  OwnerHierarchyName := 'base';
end;

function TChild.OwnerHierarchyName: string;
begin
  OwnerHierarchyName := 'derived-owner';
end;

var
  Obj: TBase;
  S: string;
begin
  Obj := TChild.Create;
  Obj.Marker := 12345;
  S := Obj.OwnerHierarchyName;
  Writeln(S);
  Writeln(Length(S));
  Writeln(Obj.Marker);
end.
