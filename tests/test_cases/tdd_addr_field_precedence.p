program tdd_addr_field_precedence;

type
  TRec = record
    value: longint;
  end;

var
  rec_ptr: ^TRec;
  field_ptr: ^longint;

begin
  new(rec_ptr);
  rec_ptr^.value := 123;
  field_ptr := @rec_ptr^.value;
  writeln(field_ptr^);
end.
