program TestExitValueRecordSret;

{ Regression test: Exit(value) in a function returning a record by SRET
  must memcpy the value into the caller's hidden output buffer instead
  of treating it as a scalar in %rax (which silently zeroed the result).

  This bug blocked pp.pas Stage 2 bootstrap: FPC's merge_classes uses
  exit(class1) / exit(class2) to return one of its record params, and
  KGPC's miscompilation propagated zeros up the classify_argument chain. }

type
  TItem = record
    p: Pointer;
    t: Byte;
  end;

function Identity(a: TItem): TItem;
begin
  exit(a);
end;

function Merge(a, b: TItem): TItem;
begin
  if a.t = 0 then exit(b);
  if b.t = 0 then exit(a);
  Result.p := nil;
  Result.t := 99;
end;

var
  x, y, r: TItem;
begin
  x.p := Pointer($DEADBEEF);
  x.t := 42;
  r := Identity(x);
  WriteLn('Identity.t=', r.t);
  WriteLn('Identity.p=', PtrUInt(r.p));

  x.p := nil; x.t := 0;
  y.p := Pointer($CAFEBABE); y.t := 7;
  r := Merge(x, y);
  WriteLn('Merge_b.t=', r.t);
  WriteLn('Merge_b.p=', PtrUInt(r.p));

  x.p := Pointer($1234); x.t := 3;
  y.p := nil; y.t := 0;
  r := Merge(x, y);
  WriteLn('Merge_a.t=', r.t);
  WriteLn('Merge_a.p=', PtrUInt(r.p));
end.
