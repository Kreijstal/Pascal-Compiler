program tdd_win_textfile_write_no_leak;
{ Regression: on Windows --no-stdlib, WriteLn(t,...) to a Rewrite'd disk
  text file must reach the file, not leak onto stdout. }
var t: Text;
begin
  Assign(t, 'wt_disk_out.txt');
  Rewrite(t);
  WriteLn(t, 'FILELINE');
  Close(t);
  WriteLn('OUTLINE');
end.
