program tdd_classref_array_virtual_constructor_returns_instance;
{$mode objfpc}

type
  TNode = class
    Value: LongInt;
    constructor Create(AValue: LongInt); virtual;
    function ReadValue: LongInt;
  end;

  TChildNode = class(TNode)
    constructor Create(AValue: LongInt); override;
  end;

  TNodeClass = class of TNode;
  TNodeClassArray = array[0..1] of TNodeClass;

constructor TNode.Create(AValue: LongInt);
begin
  inherited Create;
  Value := AValue;
end;

function TNode.ReadValue: LongInt;
begin
  ReadValue := Value;
end;

constructor TChildNode.Create(AValue: LongInt);
begin
  inherited Create(AValue + 5);
end;

var
  Classes: TNodeClassArray = (TNode, TChildNode);
  Node: TNode;

begin
  Node := Classes[1].Create(32);
  writeln(Node.ReadValue);
end.
