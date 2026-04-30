program tdd_inherited_statement_calls_parent_override;
{$mode objfpc}

type
  TBase = class
    Value: Integer;
    procedure Touch(Amount: Integer); virtual;
  end;

  TLeaf = class(TBase)
    procedure Touch(Amount: Integer); override;
  end;

procedure TBase.Touch(Amount: Integer);
begin
  Value := Amount;
end;

procedure TLeaf.Touch(Amount: Integer);
begin
  inherited;
  Value := Value + 6;
end;

var
  Leaf: TLeaf;

begin
  Leaf := TLeaf.Create;
  Leaf.Touch(17);
  WriteLn(Leaf.Value);
end.
