program fpc_bootstrap_typecast_class_field_assign;
{$mode objfpc}

type
  TDef = class
    value: LongInt;
    constructor Create(v: LongInt);
  end;

  TStoredDef = class(TDef)
  end;

  TNode = class
    resultdef: TDef;
  end;

constructor TDef.Create(v: LongInt);
begin
  value := v;
end;

var
  node: TNode;
  stored: TStoredDef;
  def: TStoredDef;

begin
  node := TNode.Create;
  stored := TStoredDef.Create(42);
  node.resultdef := stored;
  def := TStoredDef(node.resultdef);
  if (def = stored) and (def.value = 42) then
    writeln('ok')
  else
    writeln('bad');
end.
