unit dotted_alias_reexport_unit;

interface

uses dotted_alias_base_unit;

type
  pcint = dotted_alias_base_unit.pcint;

procedure takes_pcint(p: pcint);

implementation

procedure takes_pcint(p: pcint);
begin
  if p <> nil then
    p^ := p^ + 1;
end;

end.
