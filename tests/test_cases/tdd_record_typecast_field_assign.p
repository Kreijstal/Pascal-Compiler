program tdd_record_typecast_field_assign;

{$mode objfpc}

var
  Mantissa: double;

begin
  Mantissa := 1.0;
  System.TDoubleRec(Mantissa).Exp := 1;
  writeln('ok');
end.
