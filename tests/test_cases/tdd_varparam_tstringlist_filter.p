program tdd_varparam_tstringlist_filter;

uses Classes;

procedure FilterLength7(var lst: TStringList);
var
  i: integer;
begin
  for i := lst.Count - 1 downto 0 do
    if Length(lst[i]) <> 7 then
      lst.Delete(i);
end;

var
  list: TStringList;
begin
  list := TStringList.Create;
  list.Add('WORKING');
  list.Add('DYNAMICS');
  list.Add('FLOWERS');
  FilterLength7(list);
  Writeln(list.Count);
  if list.Count > 0 then
    Writeln(list[0]);
  if list.Count > 1 then
    Writeln(list[1]);
  list.Free;
end.
