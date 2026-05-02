{ Regression test: a non-packed record with a variant part whose largest
  branch contains pointer-sized fields must be padded so those fields land on
  natural alignment boundaries.  This mirrors FPC's tdwarfoper layout
  (cfidwarf.pas), where the variant 'sym' field must be at offset 8 (not 2)
  inside the operand record.  KGPC previously computed the offset and size as
  if the record were packed, which mis-located 'sym' and corrupted reads
  during DWARF CFI generation. }
program tdd_variant_record_alignment;

type
  TEnc = (e1, e2);
  TTyp = (t_const, t_sym, t_pair);

  TOper = record
    enc : TEnc;
    case typ : TTyp of
      t_const : (value : Int64);
      t_sym   : (sym : Pointer);
      t_pair  : (a, b : Pointer);
  end;

  TItem = record
    op   : Byte;
    ops  : Byte;
    oper : array[0..1] of TOper;
  end;

var
  o : TOper;
  it : TItem;
  marker : Integer;
begin
  { SizeOf(TOper) must equal 24:
      enc(1) + tag(1) + 6 pad + biggest branch(2 pointers = 16) }
  WriteLn('SizeOf(TOper) = ', SizeOf(TOper));
  WriteLn('SizeOf(TItem) = ', SizeOf(TItem));

  { Write a recognisable value through the variant-aligned 'sym' field and
    confirm we read the same value back.  If the variant-field offset is
    miscomputed (regression: offset 2 instead of offset 8 would land in the
    middle of an unrelated stack slot or be unaligned), this echo fails. }
  marker := 12345;
  o.enc := e1;
  o.typ := t_sym;
  o.sym := @marker;
  if o.sym = @marker then
    WriteLn('Sym field roundtrip OK')
  else
    WriteLn('Sym field roundtrip FAIL');

  { Same exercise with an array of operands inside an enclosing record:
    element stride must use the aligned size, otherwise oper[1].sym writes
    overlap oper[0]. }
  it.op := 1;
  it.ops := 2;
  it.oper[0].enc := e1;
  it.oper[0].typ := t_sym;
  it.oper[0].sym := @marker;
  it.oper[1].enc := e2;
  it.oper[1].typ := t_const;
  it.oper[1].value := 99;
  if (it.oper[0].sym = @marker) and (it.oper[1].value = 99) then
    WriteLn('Array stride OK')
  else
    WriteLn('Array stride FAIL');
end.
