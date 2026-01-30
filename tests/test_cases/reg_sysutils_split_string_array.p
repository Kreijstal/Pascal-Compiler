{$mode objfpc}
program reg_sysutils_split_string_array;

type
  TStringArray = array of string;

function Split(const Separators: array of char): string; overload;
begin
  Split := 'char';
end;

function Split(const Separators: array of string): string; overload;
begin
  Split := 'string';
end;

var
  sep: array[0..1] of string;
begin
  sep[0] := ',';
  sep[1] := ';';
  Writeln(Split(sep));
end.
