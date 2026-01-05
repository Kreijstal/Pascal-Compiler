{ Test case that fails with KGPC due to Free method issue }
{ 
  This test demonstrates a real difference between KGPC and FPC.
  
  The test uses a class with a Free method call, which KGPC doesn't handle correctly.
  
  Expected with KGPC: Fails with "call to procedure TMyClass__Free does not match any available overload"
  Expected with FPC: Should compile successfully
}

program fpc_bootstrap_free_method;

{$mode objfpc}

type
  TMyClass = class
  private
    FValue: Integer;
  public
    property Value: Integer read FValue write FValue;
    constructor Create; overload;
    constructor Create(aValue: Integer); overload;
  end;

{ TMyClass }

constructor TMyClass.Create;
begin
  FValue := 0;
end;

constructor TMyClass.Create(aValue: Integer);
begin
  FValue := aValue;
end;

var
  obj: TMyClass;
begin
  obj := TMyClass.Create(42);
  writeln('Value: ', obj.Value);
  obj.Free;  // This fails with KGPC
end.