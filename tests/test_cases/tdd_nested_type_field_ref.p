program tdd_nested_type_field_ref;

type
  TOuter = object
    type
      TCommonHeader = record
        h: LongInt;
      end;
      TVarHeader = record
        ch: TCommonHeader;
      end;
  end;

var
  v: TOuter.TVarHeader;

begin
  v.ch.h := 7;
  writeln(v.ch.h);
end.
