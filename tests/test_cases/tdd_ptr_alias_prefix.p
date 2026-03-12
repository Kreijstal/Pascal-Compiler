program tdd_ptr_alias_prefix;

type
  PBase = ^TBase;
  TBase = record
    Next: PBase;
  end;
  PPDerived = ^PDerived;
  PDerived = ^TDerived;
  TDerived = record
    Item: TBase;
    Tag: LongWord;
  end;

var
  EntryPtr: PPDerived;
  Entry: PDerived;

begin
  EntryPtr := @Entry^.Item.Next;
  Writeln('ok');
end.
