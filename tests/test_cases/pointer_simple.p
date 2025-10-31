program pointer_simple;
type PNode = ^TNode; TNode = record id: integer; end;
var p: PNode;
begin
  p := nil;
end.
