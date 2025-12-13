{ Test ptruint type - pointer-sized unsigned integer }
{ Required for FPC bootstrap heaptrc.pp, softfpu.pp, etc. }
program fpc_bootstrap_ptruint;

{$mode objfpc}

var
  n: ptruint;
  p: pointer;
begin
  n := ptruint(@p);
  WriteLn('ptruint size = ', SizeOf(ptruint));
  WriteLn('pointer size = ', SizeOf(pointer));
end.
