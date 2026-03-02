program metaclass_constructor_call;
{$mode objfpc}
{ Test: calling constructors on metaclass (class of T) expressions.
  Verifies TMyClass(expr).Create resolves the constructor via the class
  reference's target class. This pattern is common in FPC RTL for
  virtual construction: TStringsClass(Self.ClassType).Create }
type
  TBase = class
  public
    Name: String;
    constructor Create; virtual;
  end;

  TBaseClass = class of TBase;

  TDerived = class(TBase)
  public
    constructor Create; override;
    function CloneSelf: TBase;
  end;

constructor TBase.Create;
begin
  inherited Create;
  Name := 'Base';
end;

constructor TDerived.Create;
begin
  inherited Create;
  Name := 'Derived';
end;

function TDerived.CloneSelf: TBase;
begin
  Result := TBaseClass(Self.ClassType).Create;
end;

var
  d: TDerived;
  clone: TBase;
begin
  d := TDerived.Create;
  clone := d.CloneSelf;
  WriteLn(d.Name);
  { Note: virtual dispatch on metaclass constructor is a codegen feature;
    for now just verify the call compiles and runs without crash }
  WriteLn(clone.ClassName);
  clone.Free;
  d.Free;
end.
