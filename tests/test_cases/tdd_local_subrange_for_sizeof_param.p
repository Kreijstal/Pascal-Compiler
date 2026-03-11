program tdd_local_subrange_for_sizeof_param;
{$mode objfpc}

procedure show_range(d: longint);
type
  tbytes = array[0..sizeof(d)-1] of byte;
var
  i: 0..sizeof(d)-1;
begin
  for i := low(tbytes) to high(tbytes) do
    write(i);
  writeln;
end;

begin
  show_range($11223344);
end.
