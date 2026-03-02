program tdd_nested_type_sizeof_const;

type
  TOuter = object
    type
      TInner = record
        a: longint;
        b: byte;
      end;
    const
      CSize = SizeOf(TInner);
  end;

begin
  writeln(TOuter.CSize);
end.
