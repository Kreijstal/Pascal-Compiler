{ Regression: array['A'..'Z'] bounds must resolve to [65..90], not [0..0].
  KGPC was failing to evaluate char-literal subrange bounds in type aliases,
  causing ^SwitchRecTable to be typed as ^array[0..0] instead of ^array[65..90],
  which triggered an olevariant operator overload search and a runtime crash. }
program tdd_char_range_array_bounds;

type
  TSwitchType = (ignoredsw, localsw, modulesw);
  SwitchRec = record
    typesw : TSwitchType;
    setsw  : byte;
  end;
  SwitchRecTable = array['A'..'Z'] of SwitchRec;

const
  switchTable: SwitchRecTable = (
    (typesw:ignoredsw; setsw:0),   { A }
    (typesw:localsw;   setsw:1),   { B }
    (typesw:localsw;   setsw:2),   { C }
    (typesw:ignoredsw; setsw:0),   { D }
    (typesw:ignoredsw; setsw:0),   { E }
    (typesw:ignoredsw; setsw:0),   { F }
    (typesw:ignoredsw; setsw:0),   { G }
    (typesw:localsw;   setsw:7),   { H }
    (typesw:ignoredsw; setsw:0),   { I }
    (typesw:ignoredsw; setsw:0),   { J }
    (typesw:ignoredsw; setsw:0),   { K }
    (typesw:ignoredsw; setsw:0),   { L }
    (typesw:modulesw;  setsw:3),   { M }
    (typesw:ignoredsw; setsw:0),   { N }
    (typesw:ignoredsw; setsw:0),   { O }
    (typesw:ignoredsw; setsw:0),   { P }
    (typesw:ignoredsw; setsw:0),   { Q }
    (typesw:ignoredsw; setsw:0),   { R }
    (typesw:ignoredsw; setsw:0),   { S }
    (typesw:ignoredsw; setsw:0),   { T }
    (typesw:ignoredsw; setsw:0),   { U }
    (typesw:ignoredsw; setsw:0),   { V }
    (typesw:ignoredsw; setsw:0),   { W }
    (typesw:ignoredsw; setsw:0),   { X }
    (typesw:ignoredsw; setsw:0),   { Y }
    (typesw:ignoredsw; setsw:0)    { Z }
  );

var
  tablePtr: ^SwitchRecTable;

begin
  tablePtr := @switchTable;
  { 'H' is index 7 (0-based), so element at switchTable['H'] = localsw/setsw=7 }
  if tablePtr^['H'].typesw = localsw then
    writeln('ok')
  else
    writeln('FAIL');
end.
