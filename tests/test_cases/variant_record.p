program test_variant_record;
type
  TMyRecord = record
    case byte of
      0 : (
            field1 : integer;
          );
      1 : (
            field2 : char;
          );
  end;
begin
  writeln('test');
end.
