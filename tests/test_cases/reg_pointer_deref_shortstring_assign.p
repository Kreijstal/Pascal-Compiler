program reg_pointer_deref_shortstring_assign;

type
  PShort = ^ShortString;

function DupShort(const s: ShortString): PShort;
begin
  GetMem(Result, Length(s) + 1);
  Result^ := s;
end;

begin
  Writeln(DupShort('abc')^);
end.
