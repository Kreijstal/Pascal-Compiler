program test_shortstring_array;
type
  TNames = array[0..2] of string[19];
const
  names: TNames = ('hello', 'world', 'test');
begin
  WriteLn(names[0]);
  WriteLn(names[1]);
  WriteLn(names[2]);
end.
