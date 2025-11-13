program insert_procedure;

var
  s: string;

procedure PrintState(const labelText: string);
begin
  writeln(labelText, s);
end;

begin
  s := 'abcdef';
  Insert('ZZ', s, 2);
  PrintState('Case1=');

  s := 'abcdef';
  Insert('XYZ', s, 7); { past end }
  PrintState('Case2=');

  s := 'abcdef';
  Insert('Q', s, 0); { non-positive index }
  PrintState('Case3=');

  s := 'abcdef';
  Insert('', s, 3); { empty source }
  PrintState('Case4=');

  s := 'abcdef';
  Insert('123', s, 6); { insert before last char }
  PrintState('Case5=');
end.
