{$mode objfpc}
program string_replace_flags;

uses
  SysUtils;

var
  S: string;

begin
  S := StringReplace('Abc', 'a', 'z', [rfIgnoreCase]);
  writeln(S);
end.
