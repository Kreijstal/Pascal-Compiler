program fpc_bootstrap_shortstring_array_copy_compare;

const
  Names: array[0..2] of string[4] = ('CMOV', 'J', 'SET');

var
  s: string;
  j: LongInt;

begin
  s := 'JZ';
  j := 1;
  if Copy(s, 1, Length(Names[j])) = Names[j] then
    WriteLn('match')
  else
    WriteLn('miss');
end.
