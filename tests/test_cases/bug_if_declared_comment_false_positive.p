{ Test: declared() must ignore comments that look like declarations }
program bug_if_declared_comment_false_positive;
{$mode objfpc}

{ var Ghost: Integer; }

begin
  {$if declared(Ghost)}
  WriteLn('BUG: declared() matched a comment');
  {$else}
  WriteLn('OK');
  {$endif}
end.
