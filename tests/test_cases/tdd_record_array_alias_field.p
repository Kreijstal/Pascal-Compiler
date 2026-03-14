program tdd_record_array_alias_field;

type
  TInnerRec = packed record
    S: Word;
    L: Byte;
  end;

  TInnerAlias = TInnerRec;

  TOuter = packed record
    Index: array[0..1] of TInnerAlias;
  end;

const
  Data: TOuter = (
    Index: (
      (S: 1; L: 2),
      (S: 3; L: 4)
    )
  );

begin
  Writeln(Data.Index[0].S, ' ', Data.Index[0].L);
  Writeln(Data.Index[1].S, ' ', Data.Index[1].L);
end.
