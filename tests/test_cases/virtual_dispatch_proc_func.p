program VirtualDispatchProcFunc;

{ Tests that virtual dispatch works correctly for both:
  1. Procedure calls (statements)
  2. Function calls (expressions)
  When calling through a base class variable holding a derived instance. }

type
  TBase = class
    Value: Integer;
    procedure PrintVal; virtual;
    function GetVal: Integer; virtual;
  end;

  TDerived = class(TBase)
    procedure PrintVal; override;
    function GetVal: Integer; override;
  end;

procedure TBase.PrintVal;
begin
  WriteLn('Base: ', Value);
end;

function TBase.GetVal: Integer;
begin
  GetVal := Value;
end;

procedure TDerived.PrintVal;
begin
  WriteLn('Derived: ', Value * 10);
end;

function TDerived.GetVal: Integer;
begin
  GetVal := Value * 10;
end;

var
  Obj: TBase;
  X: Integer;
begin
  Obj := TDerived.Create;
  Obj.Value := 5;

  { Procedure call - should use dynamic dispatch }
  Obj.PrintVal;

  { Function call in expression - should use dynamic dispatch }
  X := Obj.GetVal;
  WriteLn('GetVal = ', X);
end.
