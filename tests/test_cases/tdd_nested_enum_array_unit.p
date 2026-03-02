unit tdd_nested_enum_array_unit;
interface
type
  TMyRec = record
  type
    TKind = (kkFirst, kkSecond);
  end;

function KindName(k: TMyRec.TKind): Integer;

implementation

function KindName(k: TMyRec.TKind): Integer;
begin
  if k = kkFirst then
    Result := 1
  else
    Result := 2;
end;

end.
