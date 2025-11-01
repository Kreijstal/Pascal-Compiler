program VariantTest;
type
  TKind = (a, b);
  TRecord = record
    case Kind: TKind of
      a: (I: longint);
      b: (S: string);
  end;
var
  R: TRecord;
begin
  R.Kind := a;
  R.I := 42;
  writeln(R.I);
end.
