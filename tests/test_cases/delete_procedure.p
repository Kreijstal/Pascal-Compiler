program delete_procedure;

var
  s: string;

procedure PrintState(const labelText: string);
begin
  writeln(labelText, s);
end;

begin
  s := 'abcdef';
  Delete(s, 2, 2);  { remove positions 2-3 }
  PrintState('Case1=');

  s := 'abcdef';
  Delete(s, 5, 10); { delete through end }
  PrintState('Case2=');

  s := 'abcdef';
  Delete(s, 7, 1);  { index beyond length }
  PrintState('Case3=');

  s := 'abcdef';
  Delete(s, 0, 3);  { non-positive index }
  PrintState('Case4=');

  s := 'abcdef';
  Delete(s, 3, 0);  { zero count }
  PrintState('Case5=');
end.
