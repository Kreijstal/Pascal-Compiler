program string_char_concat;
var
  source: string;
  prefix: string;
  suffix: string;
  idx: integer;
begin
  source := 'ABCDE';
  prefix := '';
  suffix := '';
  idx := 2;

  prefix := prefix + source[idx];
  suffix := source[idx + 1] + suffix;

  WriteLn('prefix=' + prefix);
  WriteLn('suffix=' + suffix);
  WriteLn('pair=' + (source[idx] + source[idx + 1]));
  WriteLn('roundtrip=' + (prefix + suffix));
end.
