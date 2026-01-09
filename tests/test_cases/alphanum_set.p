program alphanum_set;

uses
  SysUtils;

var
  C: char;

begin
  C := 'A';
  if C in AlphaNum then
    writeln('yes')
  else
    writeln('no');
end.
