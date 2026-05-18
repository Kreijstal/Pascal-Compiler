program AbstractOverrideAliasParam;

{ Regression test: override of an abstract virtual method whose parameter
  list uses a named integer-alias type must land in the SAME VMT slot as
  the parent's abstract declaration, even when the override declares the
  same parameter under a different aliased name (TRelocDataInt vs aint,
  both Int64 on 64-bit targets in FPC).

  Before the fix, the override was placed at a new slot and the parent's
  slot kept calling __kgpc_abstract_method_error at runtime, producing
  "Runtime error: Abstract method called" when dispatched via the
  parent's static type. }

type
  TRelocDataInt = Int64;
  aint = Int64;

  TBase = class
    procedure WriteData(Data: TRelocDataInt; Tag: LongInt); virtual; abstract;
    procedure Run;
  end;

  TDerived = class(TBase)
    procedure WriteData(Data: aint; Tag: LongInt); override;
  end;

procedure TBase.Run;
begin
  { Dispatched through TBase's static type — the abstract slot MUST
    have been overridden by TDerived.WriteData. }
  WriteData(42, 7);
end;

procedure TDerived.WriteData(Data: aint; Tag: LongInt);
begin
  WriteLn('WriteData ok: ', Data, ' tag=', Tag);
end;

var
  Obj: TBase;
begin
  Obj := TDerived.Create;
  Obj.Run;
end.
