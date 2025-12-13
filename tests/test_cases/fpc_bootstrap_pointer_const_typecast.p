{ Test pointer typecast in const expression - required for FPC bootstrap dl.pp }
{ dl.pp uses: RTLD_NEXT = pointer(-3); }
program fpc_bootstrap_pointer_const_typecast;

{$mode objfpc}

const
  RTLD_NEXT = pointer(-3);
  RTLD_DEFAULT = pointer(-1);

begin
  WriteLn('RTLD_NEXT = ', PtrInt(RTLD_NEXT));
  WriteLn('RTLD_DEFAULT = ', PtrInt(RTLD_DEFAULT));
end.
