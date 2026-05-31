program tdd_record_ctor_via_typename;
{ Advanced-record constructor invoked via the type name (TRec.Create(...)).
  Covers two record sizes because they take different ABI paths:
    - TSmall (8 bytes) is returned in a register;
    - TBig (16 bytes) is returned via a hidden sret pointer.
  In both, a record constructor must receive the destination as its Self and
  construct in place; the RTL TRect.Empty pattern (Result := T.Create(...)
  inside a static class function returning the record) exercises the sret
  path. Before the fix the record constructor declaration parsed as a plain
  method, so the type-name receiver leaked to codegen as
  "Unresolved non-local symbol", and once parsed as a constructor the >8-byte
  case mis-set up an sret return that shifted Self out of arg register 0. }
type
  TSmall = record
    x, y: longint;
    constructor Create(ax, ay: longint);
    class function Empty: TSmall; static;
  end;
  TBig = record
    a, b, c, d: longint;
    constructor Create(pa, pb, pc, pd: longint);
    class function Empty: TBig; static;
  end;

constructor TSmall.Create(ax, ay: longint);
begin
  x := ax; y := ay;
end;
class function TSmall.Empty: TSmall; static;
begin
  Result := TSmall.Create(0, 0);
end;

constructor TBig.Create(pa, pb, pc, pd: longint);
begin
  a := pa; b := pb; c := pc; d := pd;
end;
class function TBig.Empty: TBig; static;
begin
  Result := TBig.Create(0, 0, 0, 0);
end;

var
  s: TSmall;
  g: TBig;
begin
  s := TSmall.Create(3, 7);
  writeln('s=', s.x, ' ', s.y);
  s := TSmall.Empty;
  writeln('se=', s.x, ' ', s.y);
  g := TBig.Create(1, 2, 3, 4);
  writeln('g=', g.a, ' ', g.b, ' ', g.c, ' ', g.d);
  g := TBig.Empty;
  writeln('ge=', g.a, ' ', g.b, ' ', g.c, ' ', g.d);
end.
