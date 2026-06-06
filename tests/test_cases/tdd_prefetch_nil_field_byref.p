program tdd_prefetch_nil_field_byref;
{$mode objfpc}
{ Regression for prefetch() argument passing.

  prefetch() takes an untyped `const` argument, i.e. BY REFERENCE: FPC lowers
  `prefetch(x)` to a cache-hint on the ADDRESS of x and never dereferences it.
  KGPC's untyped-const + pointer-arg heuristic in codegen_pass_arguments passed
  such an argument BY VALUE instead, dereferencing it.

  In FPC's TLinkedList.Clear the loop does
      Next := NewNode.Next;
      prefetch(Next.Next);      { Next is nil on the last node }
      NewNode.Free;
  Passing Next.Next by value loads [Next+offset]; with Next=nil that reads
  through a near-null pointer and segfaults.  This crashed the bootstrapped
  FPC 3.2.2 compiler (pp_bootstrap) while clearing its command-line option
  list, aborting the win64 system.ppu build.  Taking the address (matching FPC)
  is crash-safe: prefetch(@(nil.field)) is just a no-op hint on a small
  constant address. }
type
  TItem = class
    Next: TItem;
    payload: longint;
    constructor Create(v: longint);
  end;

constructor TItem.Create(v: longint);
begin
  Next := nil;
  payload := v;
end;

{ Mirrors TLinkedList.Clear's prefetch-of-nil-field pattern. }
procedure DrainAndCount(head: TItem);
var
  cur, nxt: TItem;
  n: integer;
begin
  n := 0;
  cur := head;
  while assigned(cur) do
  begin
    nxt := cur.Next;
    prefetch(nxt.Next);   { nxt is nil on the final iteration }
    inc(n);
    cur.Free;
    cur := nxt;
  end;
  writeln('drained=', n);
end;

var
  head, prev, it: TItem;
  i: integer;
begin
  head := nil; prev := nil;
  for i := 1 to 4 do
  begin
    it := TItem.Create(i);
    if head = nil then head := it else prev.Next := it;
    prev := it;
  end;
  DrainAndCount(head);
  writeln('ok');
end.
