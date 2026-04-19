program fpc_bootstrap_packed_record_pointer_for;

const
  MaxInfoLen = 12;
  MaxOperands = 4;
  OT_REGISTER = 4096;

type
  TAsmOp = (A_None, A_First);

  TInsEntry = packed record
    opcode: TAsmOp;
    ops: Byte;
    optypes: array[0..MaxOperands - 1] of Int64;
    code: array[0..MaxInfoLen] of Char;
    flags: set of Byte;
  end;
  PInsEntry = ^TInsEntry;

const
  InsTab: array[0..1] of TInsEntry = (
    (
      opcode: A_None;
      ops: 0;
      optypes: (0, 0, 0, 0);
      code: (#0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0);
      flags: []
    ),
    (
      opcode: A_First;
      ops: 1;
      optypes: (OT_REGISTER, 0, 0, 0);
      code: (#222, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0, #0);
      flags: []
    )
  );

var
  Seen: LongInt;

procedure WalkTable;
var
  op: TAsmOp;
  i: LongInt;
  insentry: PInsEntry;
  exists_code: Boolean;
begin
  Seen := 0;
  for op := Low(TAsmOp) to High(TAsmOp) do
  begin
    insentry := @InsTab[Ord(op)];
    while (insentry <= @InsTab[High(InsTab)]) and (insentry^.opcode = op) do
    begin
      exists_code := False;
      for i := Low(insentry^.code) to High(insentry^.code) do
      begin
        case insentry^.code[i] of
          #222: exists_code := True;
          #0, #1, #2, #3: Break;
        end;
      end;

      for i := 0 to insentry^.ops - 1 do
      begin
        if (insentry^.optypes[i] and OT_REGISTER) = OT_REGISTER then
          Inc(Seen);
      end;

      Inc(insentry);
      if exists_code then
        Inc(Seen);
    end;
  end;
end;

begin
  WalkTable;
  WriteLn(Seen);
end.
