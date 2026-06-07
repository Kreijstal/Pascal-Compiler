program fpc_bootstrap_classref_ptr_deref_assign;
{$mode objfpc}

type
  TError = type LongInt;
  OleVariant = type LongInt;

  TDef = class
  end;

  TNode = class
    resultdef: TDef;
  end;

operator := (const source: OleVariant): TError;
begin
  Result := 0;
end;

var
  n: TNode;
  best: TDef;
  pbest: ^TDef;

begin
  n := TNode.Create;
  best := TDef.Create;
  pbest := @best;
  n.resultdef := pbest^;
  if n.resultdef = best then
    writeln('ok')
  else
    writeln('bad');
end.
