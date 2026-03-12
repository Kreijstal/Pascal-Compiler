{ Test const array with inline subrange element type }
program fpc_const_subrange_array;
{$mode objfpc}
const
  reverse_nible: array[0..15] of 0..15 =
    (0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15);
begin
  WriteLn(reverse_nible[1]);
  WriteLn(reverse_nible[5]);
end.
