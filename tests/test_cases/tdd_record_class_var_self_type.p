{ Regression test: an advanced record with a `class var` of its OWN type.
  Class vars are stored as globals, not in the instance layout, so they must
  NOT be counted when computing the record's size/alignment.  Before the fix,
  the cparser dropped the `class`/`var` keywords for advanced-record class-var
  fields, so `FMinValue: TTimeSpan` looked like an ordinary instance field of
  the record's own type, sending the sizeof/alignment walk into infinite
  self-recursion (compiler hang).  The instance size is just FTicks (Int64). }
{$mode objfpc}{$modeswitch advancedrecords}
program tdd_record_class_var_self_type;
type
  TTimeSpan = record
    FTicks: Int64;
  strict private class var
    FMinValue: TTimeSpan;
    FMaxValue: TTimeSpan;
    FZero: TTimeSpan;
  end;
begin
  WriteLn(SizeOf(TTimeSpan));
end.
