program record_exotic;

type
  TIndex = 1..3;

  { A simple nested record with a fixed-size array }
  TInner = record
    a: integer;
    arr: array[TIndex] of integer;
  end;

  { Variant record to hold either an integer or a string }
  TData = record
    kind: integer; { 0 = int, 1 = string }
    case integer of
      0: (ival: integer);
      1: (sval: string);
  end;

  { Forward declaration for recursive pointer }
  PNode = ^TNode;

  { Main node record with a pointer to the same type (linked structure) }
  TNode = record
    id: integer;
    inner: TInner;
    payload: TData;
    next: PNode;
  end;

  { A packed variant record to test packing/alignment + nested variant in a record }
  PPacked = ^TPacked;
  TPacked = packed record
    tag: byte;
    case byte of
      0: (pnum: longint);
      1: (ptext: packed array[1..16] of char);
  end;

{ Create a node with the requested id and data kind.
  Demonstrates returning a pointer, New, and setting variant fields. }
function MakeNode(id: integer; kind: integer; valueInt: integer; valueStr: string): PNode;
var
  n: PNode;
begin
  New(n);
  n^.id := id;

  { initialize the nested record }
  n^.inner.a := id * 10;
  n^.inner.arr[1] := id + 1;
  n^.inner.arr[2] := id + 2;
  n^.inner.arr[3] := id + 3;

  n^.payload.kind := kind;
  if kind = 0 then
    n^.payload.ival := valueInt
  else begin
    { assign string into variant string field }
    n^.payload.sval := valueStr;
  end;

  n^.next := nil;
  MakeNode := n;
end;

{ Modify node via pointer: demonstrates writable non-local fields through pointer }
procedure ModifyNode(n: PNode);
begin
  if n = nil then exit;
  { Use WITH to access nested members }
  with n^ do
  begin
    writeln('ModifyNode: before id=', id, ' inner.a=', inner.a);
    inner.a := inner.a + 5;
    id := id + 1000;
    if payload.kind = 0 then
      payload.ival := payload.ival * 2
    else
      payload.sval := payload.sval + ' (modified)';
    writeln('ModifyNode: after id=', id, ' inner.a=', inner.a);
  end;
end;

{ Pass nested record by value (copy) and by reference to test assignment/copy semantics }
procedure ProcessByValue(r: TInner);
begin
  { mutating r here should not change the caller's record }
  r.a := -999;
  r.arr[1] := -1;
  writeln('ProcessByValue: r.a=', r.a, ' r.arr[1]=', r.arr[1]);
end;

procedure ProcessByRef(var r: TInner);
begin
  r.a := r.a + 1;
  r.arr[2] := r.arr[2] + 10;
  writeln('ProcessByRef: r.a=', r.a, ' r.arr[2]=', r.arr[2]);
end;

{ Copy a record and demonstrate independent modifications }
procedure TestRecordAssignment;
var
  a, b: TInner;
begin
  a.a := 7;
  a.arr[1] := 11; a.arr[2] := 12; a.arr[3] := 13;

  b := a; { record assignment (copy) }
  writeln('Before modifications: a.a=', a.a, ' b.a=', b.a);
  b.a := 99;
  b.arr[1] := 101;
  writeln('After modifications: a.a=', a.a, ' a.arr[1]=', a.arr[1]);
  writeln('After modifications: b.a=', b.a, ' b.arr[1]=', b.arr[1]);
end;

{ Build a tiny cyclic linked list to test pointer references / cycles }
procedure TestLinkedList;
var
  n1, n2: PNode;
begin
  n1 := MakeNode(1, 0, 123, '');
  n2 := MakeNode(2, 1, 0, 'two');

  { Link nodes in both directions to create a small cycle }
  n1^.next := n2;
  n2^.next := n1;

  writeln('n1 id=', n1^.id, ' n2 id=', n2^.id);
  writeln('n1.inner.a=', n1^.inner.a, ' n2.inner.a=', n2^.inner.a);

  { Access variant payloads }
  if n1^.payload.kind = 0 then
    writeln('n1 payload int=', n1^.payload.ival)
  else
    writeln('n1 payload str=', n1^.payload.sval);

  if n2^.payload.kind = 1 then
    writeln('n2 payload str=', n2^.payload.sval)
  else
    writeln('n2 payload int=', n2^.payload.ival);

  { Modify via pointer }
  ModifyNode(n1);
  ModifyNode(n2);

  writeln('After ModifyNode: n1 id=', n1^.id, ' n1.inner.a=', n1^.inner.a);
  writeln('After ModifyNode: n2 id=', n2^.id, ' n2.payload.kind=', n2^.payload.kind);

  { Demonstrate with and dereference in expressions }
  with n1^ do
    writeln('With n1: inner.arr[3]=', inner.arr[3], ' next.id=', next^.id);

  { Clean up - break cycle and Dispose nodes }
  n1^.next := nil;
  n2^.next := nil;
  Dispose(n1);
  Dispose(n2);
end;

{ Test packed variant usage and copying between packed and unpacked records }
procedure TestPacked;
var
  p: PPacked;
  t: TPacked;
begin
  New(p);
  p^.tag := 1;
  p^.pnum := 2025;
  writeln('Packed as number: tag=', p^.tag, ' pnum=', p^.pnum);

  { copy into an unpacked variable and change the other field }
  t := p^;
  if t.tag = 1 then
    writeln('Copied packed: pnum=', t.pnum)
  else
    writeln('Copied packed: text=', t.ptext);

  { set packed text via array of char }
  t.tag := 1;
  t.pnum := 0;
  { Fill the character array with a recognizable pattern - ensure no overflow }
  t.ptext[1] := 'H'; t.ptext[2] := 'i'; t.ptext[3] := '!';

  writeln('Packed text prefix: ', t.ptext[1], t.ptext[2], t.ptext[3]);

  Dispose(p);
end;

var
  topInner: TInner;
  node: PNode;
begin
  writeln('--- Record exotic test start ---');

  { Test record assignment semantics }
  TestRecordAssignment;

  { Test passing records by value and by reference }
  topInner.a := 500;
  topInner.arr[1] := 1; topInner.arr[2] := 2; topInner.arr[3] := 3;
  writeln('Before ProcessByValue: topInner.a=', topInner.a, ' topInner.arr[1]=', topInner.arr[1]);
  ProcessByValue(topInner);
  writeln('After ProcessByValue: topInner.a=', topInner.a, ' topInner.arr[1]=', topInner.arr[1]);
  ProcessByRef(topInner);
  writeln('After ProcessByRef: topInner.a=', topInner.a, ' topInner.arr[2]=', topInner.arr[2]);

  { Build and exercise linked nodes with variant payloads and cycles }
  TestLinkedList;

  { Test packed/variant interactions and copying }
  TestPacked;

  { Create a small anonymous node and demonstrate record reference via @ operator }
  node := MakeNode(99, 1, 0, 'anonymous');
  writeln('Anonymous node id=', node^.id, ' payload=', node^.payload.sval);
  writeln('Address of node pointer: ', PtrUInt(node)); { print numeric pointer, FPC-compatible }
  Dispose(node);

  writeln('--- Record exotic test end ---');
end.
