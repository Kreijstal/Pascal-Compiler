program tdd_nested_brace_comment;
{ The companion unit opens with a comment containing a nested brace and must
  still compile and be usable from here. }
{$mode objfpc}{$H+}
uses
  tdd_nested_brace_comment_unit;
begin
  writeln(Answer);
end.
