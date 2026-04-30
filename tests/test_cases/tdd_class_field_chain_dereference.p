program tdd_class_field_chain_dereference;
{$mode objfpc}

type
  TChild = class
    Value: LongInt;
    function GetValue: LongInt;
    property PropValue: LongInt read GetValue;
  end;

  TParent = class
    Child: TChild;
  end;

function TChild.GetValue: LongInt;
begin
  Result := Value;
end;

var
  Parent: TParent;

begin
  Parent := TParent.Create;
  Parent.Child := TChild.Create;
  Parent.Child.Value := 3;
  WriteLn(Parent.Child.Value);
  WriteLn(Parent.Child.GetValue);
  WriteLn(Parent.Child.PropValue);
end.
