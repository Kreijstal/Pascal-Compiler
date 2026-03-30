program asm_clobber_list;
var
  x: longint;
begin
  x := 0;
  asm
    movl $42,%eax
    movl %eax,x
  end ['eax'];
  writeln(x);
end.
