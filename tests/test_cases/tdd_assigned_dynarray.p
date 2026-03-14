program tdd_assigned_dynarray;

type
  TBytes = array of Byte;

var
  data: TBytes;

begin
  if Assigned(data) then
    Writeln('bad0')
  else
    Writeln('nil');
  SetLength(data, 2);
  if Assigned(data) then
    Writeln('set')
  else
    Writeln('bad1');
end.
