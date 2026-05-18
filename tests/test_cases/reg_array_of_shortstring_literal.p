{ Regression: open-array-of-ShortString literal must materialize each slot
  with a proper {length_byte, chars} layout, not a raw 8-byte pointer.
  FPC's RTTI emit path (queue_subscriptn_multiple_by_name in aasmcnst.pas)
  passes `array of TIDString = string[127]` literals; without per-slot
  conversion the callee saw garbage length bytes and tripped internalerror
  2015071505 at globtype.pas(1001) during pp_bootstrap -> pp_stage2. }
program reg_array_of_shortstring_literal;
type
  TIDString = string[127];
var
  named: TIDString;

procedure dump(const fields: array of TIDString);
var i: integer;
begin
  for i := low(fields) to high(fields) do
    writeln('[', i, ']="', fields[i], '" len=', length(fields[i]));
end;

procedure mixed(s: TIDString);
begin
  dump(['size_start_rec',
        'typ_union_rec',
        'min_max_rec',
        'basetype_array_rec',
        s]);
end;

begin
  dump(['hello', 'world']);
  named := 'myname';
  mixed(named);
end.
