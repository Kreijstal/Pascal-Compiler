program tdd_nested_method_param_owner;

type
  TOuter = object
    type
      PNode = ^TNode;
      TNode = record
        next: PNode;
      end;
      TInner = object
        procedure Push(p: PNode);
      end;
  end;

procedure TOuter.TInner.Push(p: PNode);
begin
  if p <> nil then;
end;

procedure Test;
var
  inner: TOuter.TInner;
  node: TOuter.PNode;
begin
  node := nil;
  inner.Push(node);
  writeln('OK');
end;

begin
  Test;
end.
