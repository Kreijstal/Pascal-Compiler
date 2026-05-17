{ Regression test for the codegen_assign_record_value class-assignment
  aliasing bug.  When a class-field assignment RHS is a deeply nested
  constructor chain evaluated under heavy register pressure, the
  is_class_assignment branch of codegen_assign_record_value used to:
    1. Spill dest_reg (field address) to dest_save_slot
    2. Free dest_reg
    3. Evaluate the RHS into src_reg via codegen_expr_with_result
    4. Re-acquire dest_reg via get_free_reg
    5. Reload from dest_save_slot — but the allocator could hand the
       SAME physical register that src_reg still owns, clobbering
       src_reg's value.
    6. Emit "movq %src_reg, (%dest_reg)" — collapses to a self-store
       "movq %X, (%X)" when src_reg and dest_reg are the same physical
       register.  The destination field becomes a self-pointer.

  At the next virtual dispatch on that field (e.g. firstpass(left) in
  tvecnode.pass_1 at FPCSource/compiler/nmem.pas:1410) the VMT slot
  reads from the destination field itself, the indirect call jumps
  through NULL, and pp_bootstrap SIGSEGV's while compiling
  aasmbase.pas's ApplyAsmSymbolRestrictions.

  Fix: spill src_reg to its own slot before allocating dest_reg, then
  reload src_reg from src_save_slot into a guaranteed-distinct
  register.  Mirrors bfc48be5's record-access fix in
  codegen_stmt_calls_and_control.c, which covered codegen_var_assignment
  but missed this parallel path in codegen_stmt_assignment.c.

  This test mirrors the exact FPC pattern from nmem.pas line 1406:
    left := ctypeconvnode.create_internal(
              ccallnode.createintern('fpc_' +
                tstringdef(left.resultdef).stringtypname + '_unique',
                ccallparanode.create(
                  ctypeconvnode.create_internal(left, voidpointertype),
                  nil)),
              left.resultdef);
}

program class_field_nested_ctor_chain_assign;

{$mode objfpc}

type
  TStringDef = class
    stringtypname: ShortString;
    constructor Create(const s: ShortString);
  end;

  TNode = class
    left: TNode;
    right: TNode;
    resultdef: TStringDef;
    label_text: ShortString;
    nodeflags: longint;
    pad1, pad2, pad3, pad4: longint;
    constructor Create(const s: ShortString);
    constructor CreateConv(inner: TNode; rdef: TStringDef);
    constructor CreateCall(const name: ShortString; args: TNode);
    constructor CreatePara(arg: TNode; nextp: TNode);
  end;

  TVecNode = class(TNode)
    procedure rebuild_unique;
  end;

constructor TStringDef.Create(const s: ShortString);
begin
  stringtypname := s;
end;

constructor TNode.Create(const s: ShortString);
begin
  label_text := s;
  left := nil;
  right := nil;
  resultdef := nil;
  nodeflags := 0;
end;

constructor TNode.CreateConv(inner: TNode; rdef: TStringDef);
begin
  label_text := 'conv';
  left := inner;
  right := nil;
  resultdef := rdef;
  nodeflags := 1;
end;

constructor TNode.CreateCall(const name: ShortString; args: TNode);
begin
  label_text := name;
  left := args;
  right := nil;
  resultdef := nil;
  nodeflags := 2;
end;

constructor TNode.CreatePara(arg: TNode; nextp: TNode);
begin
  label_text := 'para';
  left := arg;
  right := nextp;
  resultdef := nil;
  nodeflags := 3;
end;

procedure TVecNode.rebuild_unique;
var
  voidp: TStringDef;
  k1, k2, k3, k4: longint;
begin
  voidp := TStringDef.Create('voidpointertype');
  k1 := 11; k2 := 22; k3 := 33; k4 := 44;
  if k1 + k2 + k3 + k4 > 0 then
  begin
    { Exact pattern from FPCSource/compiler/nmem.pas:1406-1409 }
    left := TNode.CreateConv(
      TNode.CreateCall('fpc_' + left.resultdef.stringtypname + '_unique',
        TNode.CreatePara(
          TNode.CreateConv(left, voidp),
          nil)),
      left.resultdef);
  end;
end;

var
  v: TVecNode;
  inner: TNode;
  sd: TStringDef;
begin
  sd := TStringDef.Create('ansistr');
  inner := TNode.Create('inner_original');
  inner.resultdef := sd;
  v := TVecNode.Create('vec');
  v.left := inner;
  v.resultdef := sd;
  v.rebuild_unique;
  { After rebuild, v.left should be a fresh ctypeconvnode wrapping a
    ccallnode named 'fpc_ansistr_unique'.  If the aliasing bug had hit,
    v.left would point to itself (self-VMT) and the next virtual
    dispatch / field access would crash. }
  writeln('outer = ', v.left.label_text);
  writeln('call  = ', v.left.left.label_text);
  if v.left = TNode(v) then
    writeln('BUG: self-pointer detected')
  else
    writeln('OK no self-pointer');
end.
