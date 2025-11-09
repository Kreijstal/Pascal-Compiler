program open_array_test;

{$apptype console}

procedure PrintCount(const args: array of string);
var
  i: integer;
begin
  writeln('low=', Low(args));
  writeln('high=', High(args));
  for i := Low(args) to High(args) do
    writeln(args[i]);
end;

begin
  PrintCount(['one', 'two', 'three']);
end.
