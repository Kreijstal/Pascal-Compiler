program fpc_bootstrap_ptruint_longword_align;

type
  TObjSectionOfs = PtrUInt;

function align_objsecofs(v: TObjSectionOfs; a: LongWord): TObjSectionOfs;
begin
  if a <= 1 then
    align_objsecofs := v
  else
    align_objsecofs := ((v + a - 1) div a) * a;
end;

begin
  writeln(align_objsecofs(45, 16));
end.
