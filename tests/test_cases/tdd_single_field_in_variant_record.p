program tdd_single_field_in_variant_record;
{ Regression: storing a Single (4-byte float) into a record/variant-record
  field must perform a double->single narrowing conversion, and reading it
  back must promote single->double. This mirrors FPC's tai_realconst.value
  variant record, where create_s32real assigns ts32real(value_real) into a
  union field overlapping a wider double. Previously KGPC stored the low 32
  bits of the double pattern (so 1.0 became 0.0) and read the field's raw
  single bits as if they were double bits. }
type
  ts32real = single;
  tplain = record
    s32val: ts32real;
    s64val: double;
  end;
  tvariant = record
    case integer of
      0: (s32val: ts32real);
      1: (s64val: double);
  end;
var
  p: tplain;
  v: tvariant;
  d: double;
  s: single;
begin
  d := 1.0;

  { double-typecast-to-single into a plain record field }
  p.s32val := ts32real(d);
  writeln('plain=', p.s32val:0:4);

  { double-typecast-to-single into a variant record field }
  v.s32val := ts32real(d);
  writeln('variant=', v.s32val:0:4);

  { single-var -> single field, and single field -> single field }
  s := 2.5;
  p.s32val := s;
  p.s64val := p.s32val;
  writeln('fromvar=', p.s32val:0:4);
  writeln('field2field=', p.s64val:0:4);

  { reciprocal exercising divsd through a single field (Coth-shaped) }
  d := 4.0;
  p.s32val := ts32real(d);
  writeln('recip=', (1.0 / p.s32val):0:4);
end.
