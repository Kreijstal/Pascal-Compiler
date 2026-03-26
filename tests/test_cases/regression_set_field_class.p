program regression_set_field_class;
{ Regression test: set-of-enum fields in classes must be accessible.
  Previously, PASCAL_T_SET was missing from the type-spec stop list
  in convert_class_field_decl, causing set fields to be silently dropped. }

type
  TFlag = (flagA, flagB, flagC);

  TContainer = class
  public
    data: set of TFlag;
    count: Integer;
  end;

var
  obj: TContainer;
begin
  obj := TContainer.Create;
  obj.data := [flagA, flagC];
  obj.count := 2;

  if flagA in obj.data then
    Write('A');
  if flagB in obj.data then
    Write('B');
  if flagC in obj.data then
    Write('C');
  Write(' ');
  WriteLn(obj.count);
end.
