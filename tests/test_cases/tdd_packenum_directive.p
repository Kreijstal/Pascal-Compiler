{ Regression: KGPC must honour {$PACKENUM}/{$Z}/{$MINENUMSIZE}.

  FPC's default minimum enum size is 4 bytes, but the FPC compiler itself is
  built with {$PACKENUM 1}, which packs small enums down to 1 byte.  KGPC's
  enum-size fix (enums = 4 bytes) ignored the directive, so every record that
  typecasts a 4-byte scalar onto a packed record of (word; enum; enum) — most
  notably FPC's TRegisterRec used by cgbase.newreg — got the wrong field
  offsets.  newreg then wrote the register type at offset 6 of a 4-byte
  TRegister, corrupting the value; KGPC-built pp_bootstrap SIGSEGVed in
  tcg.getcpuregister while compiling system.pp (system.inc:97, SetupEntryInfo's
  constref record param).

  This mirrors TRegisterRec exactly: under {$PACKENUM 1} the two small enums are
  1 byte each, so the packed record is 4 bytes with regtype at offset 3. }
program tdd_packenum_directive;
{$PACKENUM 1}
type
  TRegisterType = (R_NONE, R_INT, R_FPU, R_MM);   { small enum -> 1 byte }
  TSubRegister  = (SUB_NONE, SUB_L, SUB_W, SUB_D, SUB_Q); { -> 1 byte }
  TSuperRegister = type word;                       { 2 bytes }
  TRegisterRec = packed record
    supreg : TSuperRegister; { offset 0, 2 bytes }
    subreg : TSubRegister;   { offset 2, 1 byte  }
    regtype: TRegisterType;  { offset 3, 1 byte  }
  end;
var
  r: TRegisterRec;
  ok: boolean;
begin
  ok := true;
  if SizeOf(TRegisterType) <> 1 then ok := false;
  if SizeOf(TSubRegister) <> 1 then ok := false;
  if SizeOf(TRegisterRec) <> 4 then ok := false;
  if (PtrUInt(@r.supreg)  - PtrUInt(@r)) <> 0 then ok := false;
  if (PtrUInt(@r.subreg)  - PtrUInt(@r)) <> 2 then ok := false;
  if (PtrUInt(@r.regtype) - PtrUInt(@r)) <> 3 then ok := false;
  if ok then WriteLn('OK')
  else WriteLn('FAIL es=', SizeOf(TRegisterType),
               ' rs=', SizeOf(TRegisterRec),
               ' osub=', PtrUInt(@r.subreg) - PtrUInt(@r),
               ' oreg=', PtrUInt(@r.regtype) - PtrUInt(@r));
end.
