program tdd_setlength_varparam_dynarray;

type
  TIntArray = array of Integer;

procedure Resize(var a: TIntArray);
begin
  SetLength(a, 3);
end;

var
  a: TIntArray;

begin
  Resize(a);
  Writeln(Length(a));
end.
