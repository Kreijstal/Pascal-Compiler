program tdd_unit_forward_class_inherited_field;

{$mode objfpc}

uses tdd_unit_forward_class_inherited_field_unit;

var
  c: TChild;
begin
  c := TChild.Create;
  c.path := 'x';
  c.flag := true;
  c.own := 1;
  writeln(c.path);
  if c.flag then
    writeln('ok');
end.
