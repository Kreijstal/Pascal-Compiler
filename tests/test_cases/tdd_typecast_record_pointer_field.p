{$mode objfpc}
program tdd_typecast_record_pointer_field;

var
  f: Text;
begin
  TextRec(f).bufptr := @TextRec(f).buffer;
  WriteLn('OK');
end.
