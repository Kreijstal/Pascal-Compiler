{ Negative test: class of must require a class type }
program bug_classof_nonclass_target;
{$mode objfpc}

type
  TBad = class of Integer;

begin
  WriteLn('SHOULD NOT COMPILE');
end.
