program tdd_record_tail_field;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TRec = record
  private
  public
    X: Longint;
  end;

var
  R: TRec;

begin
  R.X := 1;
  writeln(R.X);
end.
