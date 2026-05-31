program tdd_dispose_typecast_property;
{ Regression: Dispose(TypeCast(List[i])) on a default-indexed property freed
  garbage.  List[i] on a class with a `default` indexed property invokes the
  getter (Get), which returns the stored pointer by value -- a non-addressable
  rvalue.  KGPC models Dispose as taking the pointer slot by reference (to nil
  it), so codegen_address_for_expr was asked for the address of the typecast.
  For a typecast of a non-addressable inner it fell through to the generic
  evaluate-as-value fallback, returning the pointer *value* in the result
  register.  Dispose then handed that value to kgpc_dispose as a void**, which
  dereferenced it (freemem(*target)) and freed garbage -> "free(): invalid
  pointer".

  Reading List[i] in a value context (writeln/freemem) always worked; only the
  by-reference Dispose path was broken.  Fixed by materialising the typecast
  rvalue into a stack temp and returning the temp's address.

  This was the FPC pp.pas bootstrap blocker after heap.inc: writing system.ppu,
  tstoreddef.buildderef runs `dispose(pderef(genericparaderefs[i]))` over a
  TFPList, which crashed the KGPC-built compiler with free(): invalid pointer. }
{$mode objfpc}
type
  pnode = ^longint;
  TMyList = class
  private
    FItems: array of pointer;
    function Get(i: longint): pointer;
  public
    cnt: longint;
    procedure Add(p: pointer);
    property Items[i: longint]: pointer read Get; default;
  end;

function TMyList.Get(i: longint): pointer;
begin
  Get := FItems[i];
end;

procedure TMyList.Add(p: pointer);
begin
  if cnt >= length(FItems) then setlength(FItems, cnt + 4);
  FItems[cnt] := p;
  inc(cnt);
end;

var
  lst: TMyList;
  p: pnode;
  i: longint;
begin
  lst := TMyList.create;
  for i := 0 to 2 do begin new(p); p^ := i * 7; lst.Add(p); end;
  for i := 0 to 2 do writeln('v', i, '=', pnode(lst[i])^);
  { the bug: typecast of a getter-backed property, by-ref to Dispose }
  for i := 0 to 2 do dispose(pnode(lst[i]));
  writeln('disposed ok');
  lst.free;
end.
