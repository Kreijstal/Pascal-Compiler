{ Regression test: in FPC-style inline asm, the construct
  <RecordTypeName>.<FieldName> must evaluate to the byte offset of that
  field within the record type — emitted as a numeric literal, not a
  symbolic reference to an external label.

  Repro of the link-time failure in pp.pas where setjump.inc emits
  `movq %rbx,jmp_buf.rbx(%rdi)` and the linker complains about an
  undefined `jmp_buf.rbx` symbol because KGPC was emitting the
  identifier verbatim. }
program tdd_inline_asm_record_field_offset;

type
  TPoint = packed record
    x, y, z, w: qword;
  end;

var
  p: TPoint;
  rx, rw: qword;

begin
  p.x := 111;
  p.y := 222;
  p.z := 333;
  p.w := 444;

  { Inline asm: read p.x (offset 0) and p.w (offset 24) using
    record-field-offset syntax. }
  asm
    leaq    p(%rip), %rcx
    movq    TPoint.x(%rcx), %rax
    movq    %rax, rx(%rip)
    movq    TPoint.w(%rcx), %rax
    movq    %rax, rw(%rip)
  end;

  writeln(rx);
  writeln(rw);
end.
