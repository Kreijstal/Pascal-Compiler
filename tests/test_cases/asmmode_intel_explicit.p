{$asmmode intel}
program asmmode_intel_explicit;

begin
  asm
    mov eax, 42
  end;
  {$asmmode gas}
  asm
    movl $7, %eax
  end;
  writeln('OK');
end.
