program tdd_constructor_self_receiver_chain;
{$mode objfpc}

type
  TNode = class
    Value: LongInt;
    constructor Create(AValue: LongInt); virtual;
    constructor CreateInternal(AValue: LongInt); virtual;
  end;

constructor TNode.Create(AValue: LongInt);
begin
  inherited Create;
  Value := AValue;
end;

constructor TNode.CreateInternal(AValue: LongInt);
begin
  Self.Create(AValue + 1);
end;

var
  Node: TNode;

begin
  Node := TNode.CreateInternal(40);
  WriteLn(Node.Value);
end.
