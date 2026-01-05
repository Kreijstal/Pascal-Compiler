{ Test case for Free method with explicit TObject inheritance }
{ 
  This test demonstrates calling the inherited Free method from TObject.
  
  Expected with KGPC: Should compile and output "Value: 42"
  Expected with FPC: Should compile successfully
}

program fpc_bootstrap_free_method;

{$mode objfpc}

type
  TMyClass = class(TObject)
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
  obj.Free;
end.