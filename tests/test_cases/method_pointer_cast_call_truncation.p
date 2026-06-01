program method_pointer_cast_call_truncation;

{ Regression: calling through a typecast-to-"procedure of object" of a
  TMethod-style record (e.g. FPC's `tprocedureofobject(r)()` in
  ttypeconvnode.second_call_helper) must load the 8-byte code pointer
  from the record's first qword with a 64-bit `movq`.  The indirect-call
  path only set load_from_memory for record/array/deref/var callees, so a
  EXPR_TYPECAST callee fell through to codegen_evaluate_expr, which strips
  the cast and materialises the inner record value with a 32-bit `movl`,
  truncating the 64-bit code pointer.  The call then jumped to the low 32
  bits of the address.  Surfaced fatally on Win64 (high code addresses);
  the fix unwraps the typecast to its inner addressable expression so the
  descriptor's code@0 / data@8 are loaded with `movq`.

  Validated on Linux: the method runs and Self (Data@8) is bound, so
  Tag(35)+7 = 42 is printed. }

type
  TMethod = record
    Code: Pointer;
    Data: Pointer;
  end;
  TProc = procedure of object;

  TObj = class
    Tag: Integer;
    procedure Bump;
  end;

var
  gResult: Integer;

procedure TObj.Bump;
begin
  gResult := Tag + 7;
end;

const
  procs: array[0..0] of pointer = (@TObj.Bump);

var
  o: TObj;
  r: TMethod;
begin
  o := TObj.Create;
  o.Tag := 35;
  r.Code := procs[0];
  r.Data := o;
  gResult := 0;
  TProc(r)();
  writeln(gResult);
end.
