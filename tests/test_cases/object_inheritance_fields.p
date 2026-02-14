program object_inheritance_fields;
{ Test that object type inheritance correctly resolves fields
  from parent objects, including multi-level inheritance chains
  and access through pointer types (like FPC's heap.inc patterns). }

type
  pBase = ^TBase;
  TBase = object
    size: LongInt;
    tag: Byte;
  end;

  pChild = ^TChild;
  TChild = object(TBase)
    prev: pChild;
    next: pChild;
  end;

  pGrandChild = ^TGrandChild;
  TGrandChild = object(TChild)
    extra: Integer;
  end;

var
  b: TBase;
  c: TChild;
  g: TGrandChild;
  pc: pChild;
  pg: pGrandChild;
begin
  { Direct field access on base }
  b.size := 100;
  b.tag := 7;
  WriteLn(b.size);
  WriteLn(b.tag);

  { Inherited field access on child }
  c.size := 200;
  c.tag := 3;
  c.prev := nil;
  c.next := nil;
  WriteLn(c.size);
  WriteLn(c.tag);

  { Multi-level inherited field access on grandchild }
  g.size := 300;
  g.tag := 5;
  g.prev := nil;
  g.next := nil;
  g.extra := 42;
  WriteLn(g.size);
  WriteLn(g.tag);
  WriteLn(g.extra);

  { Pointer dereference with inherited fields }
  pc := @c;
  pc^.size := 999;
  WriteLn(pc^.size);

  pg := @g;
  pg^.size := 888;
  pg^.extra := 77;
  WriteLn(pg^.size);
  WriteLn(pg^.extra);
end.
