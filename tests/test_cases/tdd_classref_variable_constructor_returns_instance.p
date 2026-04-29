program tdd_classref_variable_constructor_returns_instance;
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
  NodeClass: TNodeClass;
  Node: TNode;

begin
  NodeClass := TChildNode;
  Node := NodeClass.Create(32);
  writeln(Node.ReadValue);
end.
