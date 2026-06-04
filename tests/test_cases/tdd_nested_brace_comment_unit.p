{ Regression: this unit opens with a comment that contains a nested,
  directive-shaped {$i foo.inc} token mentioned in prose.  Because brace
  comments nest in FPC, the inner brace must not close the comment early.
  KGPC's unit/program detector used a non-nesting comment skip that stopped
  at the inner closing brace, mis-saw this file as not starting with the unit
  keyword, parsed it with the program grammar and failed.  See also the
  AST-cache include-path test which depends on this nesting. }
unit tdd_nested_brace_comment_unit;
{$mode objfpc}{$H+}
interface

function Answer: Integer;

implementation

function Answer: Integer;
begin
  Answer := 42;
end;

end.
