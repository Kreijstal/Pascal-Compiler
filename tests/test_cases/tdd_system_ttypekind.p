program tdd_system_ttypekind;

const
  FirstKind = Low(System.TTypeKind);
  LastKind = High(System.TTypeKind);
  UnknownKind = System.tkUnknown;

begin
  writeln(Ord(FirstKind), ' ', Ord(LastKind), ' ', Ord(UnknownKind));
end.
