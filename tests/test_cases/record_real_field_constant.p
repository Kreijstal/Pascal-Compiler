program record_real_field_constant;

type
  complex = record
    R, I: real;
  end;

const
  C: complex = (R: 3; I: 1.5);

begin
  writeln(C.R);
  writeln(C.I);
end.
