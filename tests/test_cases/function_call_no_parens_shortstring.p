program FunctionCallNoParensShortString;

function TimeZoneDir: shortstring;
begin
  TimeZoneDir := 'dir/';
end;

var
  fn: shortstring;

begin
  fn := 'file';
  writeln(TimeZoneDir + fn);
end.
