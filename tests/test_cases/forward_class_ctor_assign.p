program forward_class_ctor_assign;

{$mode objfpc}

type
  TObject = class
  end;

  TFoo = class;

  TFoo = class
  public
    constructor Create;
  end;

constructor TFoo.Create;
begin
end;

var
  A: TFoo;

begin
  A := TFoo.Create;
  if A <> nil then
    writeln('ok')
  else
    writeln('nil');
end.
