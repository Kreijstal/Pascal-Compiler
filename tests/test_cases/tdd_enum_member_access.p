program tdd_enum_member_access;

uses ObjPas;

type
  TEndian = ObjPas.TEndian;

const
  CPUEndian = TEndian.Little;

begin
  if CPUEndian = TEndian.Little then
    writeln('little');
end.
