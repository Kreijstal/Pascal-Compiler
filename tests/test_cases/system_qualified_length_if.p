program system_qualified_length_if;

var
  s: string;

begin
  s := '';
  if System.Length(s) > 0 then
    s := 'x';
end.
