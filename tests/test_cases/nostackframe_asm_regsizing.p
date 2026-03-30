program nostackframe_asm_regsizing;

function atomic_cmp_xchg_8(var Target: shortint; NewValue: shortint; Comparand: shortint): shortint; assembler; nostackframe;
asm
    movl %edx,%eax
    lock
    cmpxchgb NewValue,(%rdi)
end;

function atomic_xadd_32(var Target: longint; Value: longint): longint; assembler; nostackframe;
asm
    lock
    xaddl Value,(%rdi)
    movl Value,%eax
end;

var
  x: shortint;
  y: longint;
begin
  x := 10;
  writeln(atomic_cmp_xchg_8(x, 20, 10));
  writeln(x);
  y := 100;
  writeln(atomic_xadd_32(y, 5));
  writeln(y);
end.
