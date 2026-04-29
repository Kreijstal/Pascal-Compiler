program tdd_classref_nested_constructor_argument_instance;
{$mode objfpc}

type
  TNodeKind = (nkBase, nkStatement, nkBlock);

  TNode = class
    Kind: TNodeKind;
    constructor Create(AKind: TNodeKind); virtual;
  end;

  TUnaryNode = class(TNode)
    Left: TNode;
    constructor Create(AKind: TNodeKind; ALeft: TNode); virtual;
  end;

  TStatementNode = class(TUnaryNode)
    Right: TNode;
    constructor Create(ALeft, ARight: TNode); virtual;
  end;

  TBlockNode = class(TUnaryNode)
    constructor Create(ALeft: TNode); virtual;
  end;

  TStatementNodeClass = class of TStatementNode;
  TBlockNodeClass = class of TBlockNode;

constructor TNode.Create(AKind: TNodeKind);
begin
  inherited Create;
  Kind := AKind;
end;

constructor TUnaryNode.Create(AKind: TNodeKind; ALeft: TNode);
begin
  inherited Create(AKind);
  Left := ALeft;
end;

constructor TStatementNode.Create(ALeft, ARight: TNode);
begin
  inherited Create(nkStatement, ALeft);
  Right := ARight;
end;

constructor TBlockNode.Create(ALeft: TNode);
begin
  inherited Create(nkBlock, ALeft);
end;

var
  CStatementNode: TStatementNodeClass;
  CBlockNode: TBlockNodeClass;
  Block: TBlockNode;

begin
  CStatementNode := TStatementNode;
  CBlockNode := TBlockNode;
  Block := CBlockNode.Create(CStatementNode.Create(nil, nil));
  writeln(Ord(Block.Kind));
  writeln(Ord(TStatementNode(Block.Left).Kind));
end.
