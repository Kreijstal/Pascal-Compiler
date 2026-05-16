{ Regression test: a 2-D typed const declared inside a function must reserve
  full storage for both dimensions.

  KGPC's .comm size for typed consts came from
  kgpc_type_get_array_dimension_info on the FindSymbol-returned KgpcType.  For
  function-local typed consts the lookup returned a single-dim type, so
  total_size was outer_length * scalar_element_size — short by the product of
  the inner dimensions.  The runtime init loop then wrote past the end of
  .comm storage and clobbered the next BSS symbol.  In pp.pas this corrupted
  cpubase.flags_to_cond's lookup table and crashed every integer compare with
  internalerror 2014041302. }
program typed_const_2d_array_size;
{$mode objfpc}

const
  NEXT_GUARD: longint = 0;  { adjacent BSS we want to keep at zero }

function lookup(row, col: longint): longint;
const
  table: array[0..3, 0..3] of longint =
    ((1,2,3,4),(5,6,7,8),(9,10,11,12),(13,14,15,16));
begin
  Result := table[row, col];
end;

begin
  if (lookup(0, 0) <> 1) or
     (lookup(0, 3) <> 4) or
     (lookup(3, 0) <> 13) or
     (lookup(3, 3) <> 16) or
     (lookup(2, 1) <> 10) then
  begin
    writeln('FAIL');
    halt(1);
  end;
  if NEXT_GUARD <> 0 then
  begin
    writeln('FAIL: adjacent BSS clobbered, NEXT_GUARD=', NEXT_GUARD);
    halt(1);
  end;
  writeln('ok');
end.
