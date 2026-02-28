program asm_const_equ;

const
  MagicValue = 1234;

begin
  asm
    cmp $MagicValue, %eax
  end;
  writeln('OK');
end.
