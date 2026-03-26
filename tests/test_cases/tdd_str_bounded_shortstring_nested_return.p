{$H+}
program TDDStrBoundedShortStringNestedReturn;

function FormatNumber: string;
var
  S: string[16];
begin
  Str(397, S);
  FormatNumber := S;
end;

begin
  Writeln(FormatNumber);
end.
