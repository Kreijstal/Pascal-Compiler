program tdd_low_high_enum_var_for_loop;

{$mode objfpc}

type
  TLoopEnum = (leFirstValue, leSecondValue, leThirdValue);

var
  LoopValue: TLoopEnum;
begin
  for LoopValue := Low(LoopValue) to High(LoopValue) do
    WriteLn(Ord(LoopValue));
end.
