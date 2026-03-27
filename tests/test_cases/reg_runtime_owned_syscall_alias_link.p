program reg_runtime_owned_syscall_alias_link;

function FpSysCall(sysnr: PtrInt): PtrInt; assembler; nostackframe; [public,alias:'FPC_SYSCALL0'];
begin
end;

begin
  writeln(123);
end.
