program string_concat_char_indexing;
var
  Input: string;
  Suffix, Prefix: string;
begin
  Input := '123.45';

  Suffix := '';
  Suffix := Suffix + Input[2];
  Suffix := Suffix + Input[4];
  Suffix := Suffix + Input[6];
  WriteLn('Suffix build: ', Suffix);

  Prefix := 'core';
  Prefix := Input[1] + Prefix;
  Prefix := Input[3] + Prefix;
  WriteLn('Prefix build: ', Prefix);
end.
