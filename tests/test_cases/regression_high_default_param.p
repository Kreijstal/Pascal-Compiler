{ Regression test: High() as default parameter value
  Verifies that High(SomeType) can be used as a default
  parameter value in a procedure/function declaration.

  This pattern is used in heaptrc.pp (CheckHeap parameter default).
}
{$mode objfpc}
program regression_high_default_param;

type
  TMyInt = LongInt;

procedure ShowMax(limit: TMyInt = High(LongInt));
begin
  WriteLn('limit=', limit);
end;

begin
  ShowMax;
  ShowMax(100);
end.
