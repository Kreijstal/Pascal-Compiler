program format_function;

uses SysUtils;

var
  output: string;
begin
  output := Format('Name=%s Value=%d Price=%.2f Char=%c Hex=%x',
    ['Widget', 42, 3.14159, 'A', 255]);
  writeln(output);
end.
