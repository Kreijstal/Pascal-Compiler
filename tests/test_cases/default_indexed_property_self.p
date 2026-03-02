program test_default_indexed_property_self;
{$mode objfpc}
{ Test: default indexed property access via Self and external object.
  Exercises: property Items[Index]: String read GetItem write SetItem; default;
  Ensures Self[i] and obj[i] both correctly resolve to the getter return type (String).
  Also tests that Add(Self[i]) matches Add(const S: String) overload. }
type
  TMyList = class
  private
    FItems: array[0..9] of String;
    FCount: Integer;
    function GetItem(Index: Integer): String;
    procedure SetItem(Index: Integer; const Value: String);
  public
    property Items[Index: Integer]: String read GetItem write SetItem; default;
    procedure Add(const S: String);
    procedure CopyTo(Other: TMyList);
    property Count: Integer read FCount;
  end;

function TMyList.GetItem(Index: Integer): String;
begin
  Result := FItems[Index];
end;

procedure TMyList.SetItem(Index: Integer; const Value: String);
begin
  FItems[Index] := Value;
end;

procedure TMyList.Add(const S: String);
begin
  FItems[FCount] := S;
  Inc(FCount);
end;

procedure TMyList.CopyTo(Other: TMyList);
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    Other.Add(Self[i]);
end;

var
  src, dst: TMyList;
begin
  src := TMyList.Create;
  dst := TMyList.Create;
  src.Add('Hello');
  src.Add('World');
  src.Add('Test');
  src.CopyTo(dst);
  WriteLn(dst[0]);
  WriteLn(dst[1]);
  WriteLn(dst[2]);
  WriteLn(dst.Count);
  dst.Free;
  src.Free;
end.
