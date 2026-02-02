{ TDD: record type alias should be compatible with parameter of base record type }
{$mode objfpc}
program tdd_record_alias_param;

type
  TRec = record
    A: Integer;
  end;
  TAlias = TRec;

procedure Touch(var R: TRec);
begin
  R.A := R.A + 1;
end;

var
  X: TAlias;

begin
  X.A := 1;
  Touch(X);
  WriteLn(X.A);
end.
