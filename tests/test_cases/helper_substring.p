{$mode delphi}
program helper_substring;

uses
  SysUtils;

var
  S: string;

begin
  S := 'hello';
  writeln(S.Substring(2));
end.
