program tdd_assembler_func_return;
{$mode objfpc}
{$asmmode att}

{ Framed (no nostackframe) assembler function whose body sets the return
  value in %rax.  Regression: KGPC used to emit a result-load epilogue after
  the asm block for non-nostackframe assembler functions, clobbering %rax.
  This is the construct FPC's RTL FPC_SYSCALL6 (heap manager mmap path) uses. }

{ Returns its first argument (passed in %rdi). }
function ret_first(a, b, c: int64): int64; assembler;
asm
  movq a, %rax
end;

{ Returns its seventh argument, which is passed on the stack at 16(%rbp). }
function ret_seventh(a, b, c, d, e, f, g: int64): int64; assembler;
asm
  movq g, %rax
end;

begin
  writeln(ret_first(11, 22, 33));
  writeln(ret_seventh(1, 2, 3, 4, 5, 6, 777));
end.
