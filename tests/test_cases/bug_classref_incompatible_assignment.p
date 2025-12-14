{ Negative test: unrelated class assigned to "class of" variable must be rejected }
program bug_classref_incompatible_assignment;
{$mode objfpc}

type
  TA = class
  end;

  TB = class
  end;

  TARef = class of TA;

var
  C: TARef;

begin
  C := TB;
  WriteLn('SHOULD NOT COMPILE');
end.
