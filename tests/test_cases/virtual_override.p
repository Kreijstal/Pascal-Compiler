program TestVirtualOverride;

{ Test case for virtual method override - static dispatch version }
{ Note: This test demonstrates that override syntax is accepted and compiles.
  True dynamic dispatch (polymorphism) requires VMT which is not yet fully implemented. }

type
  TBase = class
    Value: Integer;
    procedure Show; virtual;
  end;

  TDerived = class(TBase)
    Extra: Integer;
    procedure Show; override;
  end;

procedure TBase.Show;
begin
  WriteLn('TBase.Show: Value=', Value);
end;

procedure TDerived.Show;
begin
  WriteLn('TDerived.Show: Value=', Value, ', Extra=', Extra);
end;

var
  D: TDerived;
begin
  { Test with derived class - override method should be called }
  D := TDerived.Create;
  D.Value := 20;
  D.Extra := 30;
  D.Show;
end.
