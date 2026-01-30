{$mode objfpc}
program reg_overload_array_elem_shortstring_vs_ansichar;

function Which(const sep: array of AnsiChar): Integer; overload;
begin
  Which := 1;
end;

function Which(const sep: array of ShortString): Integer; overload;
begin
  Which := 2;
end;

var
  sepChars: array[0..1] of AnsiChar;
  sepStrs: array[0..1] of ShortString;
begin
  sepChars[0] := ',';
  sepChars[1] := ';';
  sepStrs[0] := ',';
  sepStrs[1] := ';';
  Writeln(Which(sepChars));
  Writeln(Which(sepStrs));
end.
