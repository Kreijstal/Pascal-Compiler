program tdd_classref_named_virtual_constructor_returns_instance;
{$mode objfpc}

type
  TNode = class
    Value: LongInt;
    constructor Load(AValue: LongInt); virtual;
    function ReadValue: LongInt;
  end;

  TChildNode = class(TNode)
    constructor Load(AValue: LongInt); override;
  end;

  TNodeClass = class of TNode;
  TNodeClassArray = array[0..1] of TNodeClass;

constructor TNode.Load(AValue: LongInt);
begin
  inherited Create;
  Value := AValue;
end;

function TNode.ReadValue: LongInt;
begin
  ReadValue := Value;
end;

constructor TChildNode.Load(AValue: LongInt);
begin
  inherited Load(AValue + 5);
end;

var
  Classes: TNodeClassArray = (TNode, TChildNode);
  Node: TNode;

begin
  Node := Classes[1].Load(32);
  writeln(Node.ReadValue);
end.
