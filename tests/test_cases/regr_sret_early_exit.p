program regr_sret_early_exit;

{$mode objfpc}

type
  TBigRec = record
    a, b, c, d, e, f: Integer;
  end;

function MakeRec(early: boolean): TBigRec;
begin
  Result.a := 1;
  Result.b := 2;
  if early then Exit;
  Result.c := 3;
  Result.d := 4;
  Result.e := 5;
  Result.f := 6;
end;

var
  r: TBigRec;
begin
  r := MakeRec(true);
  writeln(r.a, ' ', r.b);
  r := MakeRec(false);
  writeln(r.a, ' ', r.b, ' ', r.c, ' ', r.d, ' ', r.e, ' ', r.f);
end.
