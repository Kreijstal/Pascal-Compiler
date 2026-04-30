program tdd_typename_shortstring_property_concat;
{$mode objfpc}

type
  TEntry = class
  private
    FRealName: PShortString;
    function GetRealName: ShortString;
  public
    property RealName: ShortString read GetRealName;
    procedure SetName(const S: ShortString);
  end;

  TDef = class
    Entry: TEntry;
    function OwnerHierarchyName: string; virtual;
    function GetTypeName: string; virtual;
    function TypeName: string;
  end;

function TEntry.GetRealName: ShortString;
begin
  Result := FRealName^;
end;

procedure TEntry.SetName(const S: ShortString);
begin
  New(FRealName);
  FRealName^ := S;
end;

function TDef.OwnerHierarchyName: string;
begin
  Result := '';
end;

function TDef.GetTypeName: string;
begin
  Result := 'fallback';
end;

function TDef.TypeName: string;
begin
  Result := OwnerHierarchyName;
  if Assigned(Entry) and (Entry.RealName[1] <> '$') then
    Result := Result + Entry.RealName
  else
    Result := Result + GetTypeName;
end;

var
  D: TDef;

begin
  D := TDef.Create;
  D.Entry := TEntry.Create;
  D.Entry.SetName('TEXCEPTADDR');
  Writeln(D.TypeName);
end.
