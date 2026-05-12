program tdd_char_indexed_array_ptr;
type
  TRec = record
    kind: byte;
    val: byte;
  end;
  TTable = array['A'..'Z'] of TRec;
const
  myTable: TTable = (
    {A} (kind: 1; val: 0),
    {B} (kind: 2; val: 1),
    {C} (kind: 3; val: 2),
    {D} (kind: 4; val: 3),
    {E} (kind: 5; val: 4),
    {F} (kind: 6; val: 5),
    {G} (kind: 7; val: 6),
    {H} (kind: 8; val: 7),
    {I} (kind: 9; val: 8),
    {J} (kind: 10; val: 9),
    {K} (kind: 0; val: 0),
    {L} (kind: 0; val: 0),
    {M} (kind: 0; val: 0),
    {N} (kind: 0; val: 0),
    {O} (kind: 0; val: 0),
    {P} (kind: 0; val: 0),
    {Q} (kind: 0; val: 0),
    {R} (kind: 0; val: 0),
    {S} (kind: 0; val: 0),
    {T} (kind: 0; val: 0),
    {U} (kind: 0; val: 0),
    {V} (kind: 0; val: 0),
    {W} (kind: 0; val: 0),
    {X} (kind: 0; val: 0),
    {Y} (kind: 0; val: 0),
    {Z} (kind: 0; val: 0)
  );
var
  tablePtr: ^TTable;
  c: char;
begin
  tablePtr := @myTable;
  c := 'H';
  writeln(tablePtr^[c].kind);
end.
