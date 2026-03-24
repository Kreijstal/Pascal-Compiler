program transitive_dep_inherit;

{ Test 3-level transitive unit dependency for parent class resolution.
  transitive_dep_base defines TGrandparent.
  transitive_dep_middle uses base and defines TParent = class(TGrandparent).
  This program uses middle (but NOT base) and defines TChild = class(TParent).
  TChild must resolve TGrandparent transitively through middle -> base. }

uses transitive_dep_middle;

type
  TChild = class(TParent)
    FChildVal: Integer;
    constructor Create;
  end;

constructor TChild.Create;
begin
  inherited Create;
  FChildVal := 30;
end;

var
  c: TChild;
begin
  c := TChild.Create;
  WriteLn(c.FBaseVal);
  WriteLn(c.FMiddleVal);
  WriteLn(c.FChildVal);
  c.Free;
end.
