unit tdd_inline_asm_label_jmp_dce_unit;

interface

function CallerAsm(x: longint): longint;

implementation

function HelperBody(x: longint): longint; forward;

{ Inline-asm tail-call to HelperBody via a label-prefixed jmp.  Without
  the DCE fix, HelperBody's body is dropped because the asm scanner
  misreads `.LSkip:` as the mnemonic. }
function CallerAsm(x: longint): longint; assembler; nostackframe;
asm
  cmpq $0, %rcx
  je .LSkip
.LSkip: jmp HelperBody
end;

function HelperBody(x: longint): longint;
begin
  Result := x + 1000;
end;

end.
