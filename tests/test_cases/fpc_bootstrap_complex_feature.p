{ Test case for complex FPC features that KGPC may not support }
{ 
  This test attempts to use features that may not be fully supported in KGPC
  but are supported in FPC.
  
  Expected with KGPC: May fail depending on feature support
  Expected with FPC: Should compile successfully
}

program fpc_bootstrap_complex_feature;

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
  obj.Free;
end.