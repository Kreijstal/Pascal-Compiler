program tdd_large_set_field_alignment;
{$mode objfpc}{$H+}
{ Regression: a `set of <enum with >= 32 members>` is 32 bytes in FPC and is
  aligned to the pointer size (8) inside a record, not 4.  KGPC formerly
  aligned such an anonymous set field to 4, shifting every following field and
  breaking layout-sensitive RTL records (e.g. tsysteminfo.first_parm_offset in
  the FPC compiler), which corrupted syscall stack-argument offsets. }
type
  tflag = (f0,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,
           f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31,
           f32,f33,f34,f35,f36,f37,f38,f39,f40);
  trec = record
    b: byte;
    flags: set of tflag;
    after: longint;
  end;
var r: trec;
begin
  writeln('off_flags=', PtrUInt(@r.flags) - PtrUInt(@r));
  writeln('off_after=', PtrUInt(@r.after) - PtrUInt(@r));
  writeln('sizeof_rec=', SizeOf(r));
end.
