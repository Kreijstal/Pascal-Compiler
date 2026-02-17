program tdd_typed_pointer_alias_method_call;
{$mode objfpc}

type
  PNode = ^TNode;
  TNode = record
    value: LongInt;
    next: PNode;
  end;

  PThreadState = type ^TThreadState;
  TThreadState = object
    counter: LongInt;
    procedure PushToFree(p: PNode);
  end;

procedure TThreadState.PushToFree(p: PNode);
begin
  if p <> nil then
    counter := counter + p^.value;
end;

var
  ts: TThreadState;
  ts_ptr: PThreadState;
  ts_ptr_ptr: ^PThreadState;
  node: TNode;
begin
  ts.counter := 0;
  node.value := 7;
  node.next := nil;

  ts_ptr := @ts;
  ts_ptr_ptr := @ts_ptr;
  ts_ptr_ptr^^.PushToFree(@node);

  writeln(ts.counter);
end.
