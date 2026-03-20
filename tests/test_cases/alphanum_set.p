program alphanum_set;

uses
  SysUtils;

const
  AlphaNum = ['A'..'Z', 'a'..'z', '_', '0'..'9'];

var
  C: char;

begin
  C := 'A';
  if C in AlphaNum then
    writeln('yes')
  else
    writeln('no');
end.
