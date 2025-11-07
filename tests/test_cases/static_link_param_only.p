program StaticLinkParamOnly;
var
  x: integer;

procedure SetX(value: integer);
begin
  x := value;
end;

begin
  x := 0;
  SetX(5);
  writeln(x);
end.
