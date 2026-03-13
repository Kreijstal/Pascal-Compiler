program const_array_char_matrix_rows;

{$mode objfpc}

const
  Msgs: array[0..1, 1..8] of char = (
    'abc'#0,
    'xyz'
  );

begin
  writeln(Msgs[0,1], Msgs[0,2], Msgs[0,3], ' ', Ord(Msgs[0,4]), ' ', Ord(Msgs[0,8]));
  writeln(Msgs[1,1], Msgs[1,2], Msgs[1,3], ' ', Ord(Msgs[1,4]), ' ', Ord(Msgs[1,8]));
end.
