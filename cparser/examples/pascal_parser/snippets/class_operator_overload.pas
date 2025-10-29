unit ClassOperatorDemo;
interface
type
  TFoo = class
  public
    class operator Equal(const A, B: TFoo): Boolean;
  end;
implementation
class operator TFoo.Equal(const A, B: TFoo): Boolean;
begin
end;
end.
