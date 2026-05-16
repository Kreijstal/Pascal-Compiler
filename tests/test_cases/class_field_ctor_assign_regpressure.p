{ Regression test for register-aliasing bug in class field assignment.

  When a class-field assignment RHS is a constructor call evaluated
  under heavy register pressure, codegen_assign_record_value used to
  free dest_reg before evaluating the RHS, then re-acquire dest_reg
  via get_free_reg.  If src_reg returned by codegen_expr_with_result
  happened to live in the same physical register that was freed at
  the start, the subsequent dest reload would clobber src_reg's
  value, and the final "movq %src, (%dest)" collapsed into
  "movq %X, (%X)" — making the field point to itself.  This produced
  a self-VMT-pointer that crashed at the next virtual dispatch.

  The fix routes the final pointer store through %r11 (caller-saved
  scratch never in the allocatable pool), reading the dest address
  straight out of its spill slot, so src_reg's value is preserved.

  Mirrors the cfileutl.pas bstoslash() pattern that triggered the
  pp_bootstrap SIGSEGV: deeply nested constructor chain inside an
  AnsiString unique-call (via vnf_callunique) where tvecnode.pass_1
  rebuilds the left subtree as
    left := ctypeconvnode.create_internal(
              ccallnode.createintern(
                ...,
                ccallparanode.create(
                  ctypeconvnode.create_internal(left,voidpointertype),
                  nil)),
              left.resultdef);
}

program class_field_ctor_assign_regpressure;

{$mode objfpc}

type
  TNode = class
    left: TNode;
    right: TNode;
    label_text: ShortString;
    constructor Create(const s: ShortString);
  end;

constructor TNode.Create(const s: ShortString);
begin
  label_text := s;
end;

procedure rebuild(self: TNode);
var
  a, b, c, d, e: ShortString;
begin
  a := 'A';
  b := 'B';
  c := 'C';
  d := 'D';
  e := 'E';
  { Multiple nested constructor calls inside a single field assignment
    pressure the register allocator into spilling around the call. }
  self.left := TNode.Create(a + b + c + d + e);
end;

var
  root: TNode;
begin
  root := TNode.Create('root');
  rebuild(root);
  { Touch root.left.label_text — would crash with the old codegen
    because root.left would point to itself (self-VMT corruption). }
  writeln(root.left.label_text);
end.
