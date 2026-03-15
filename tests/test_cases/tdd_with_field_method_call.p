{$mode objfpc}
program tdd_with_field_method_call;

type
  TInner = class
    function Name: string;
  end;

  TOuter = class
  public
    FData: TInner;
  end;

function TInner.Name: string;
begin
  Result := 'inner';
end;

var
  p: TOuter;
begin
  p := TOuter.Create;
  p.FData := TInner.Create;
  with p do
    WriteLn(FData.Name);
end.
