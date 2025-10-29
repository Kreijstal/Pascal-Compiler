unit ClassFunctionDemo;
interface
type
  TFoo = class
  public
    class function CreateDefault: TFoo;
  end;
implementation
class function TFoo.CreateDefault: TFoo;
begin
end;
end.
