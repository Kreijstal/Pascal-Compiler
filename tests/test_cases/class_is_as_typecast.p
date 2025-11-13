program ClassIsAsTypecast;

type
  TMyClass = class
    Value: Integer;
    procedure Report;
  end;

  TMyClassDescendant = class(TMyClass)
    Extra: Integer;
    procedure Report;
    procedure DescOnly;
  end;

procedure TMyClass.Report;
begin
  WriteLn('Base value=', Value);
end;

procedure TMyClassDescendant.Report;
begin
  WriteLn('Desc value=', Value, ', extra=', Extra);
end;

procedure TMyClassDescendant.DescOnly;
begin
  WriteLn('DescOnly sees extra=', Extra);
end;

var
  Base: TMyClass;
  Desc: TMyClassDescendant;
begin
  Base.Value := 5;
  Desc.Value := 42;
  Desc.Extra := 7;

  if Base is TMyClass then
    Base.Report;

  if Base is TMyClassDescendant then
    WriteLn('Base unexpectedly a descendant')
  else
    WriteLn('Base not descendant');

  Base := Desc;
  if Base is TMyClassDescendant then
  begin
    WriteLn('Base now treated as descendant');
    (Base as TMyClassDescendant).DescOnly;
  end;
end.
