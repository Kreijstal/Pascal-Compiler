{ TDD: Array indexed by subrange type whose bound is a const expression
  Pattern from FPC compiler: tregisterindex=0..CONST_EXPR-1;
  followed by: array[tregisterindex] of ... = (...) }
program tdd_const_array_computed_range_type;

const
  NUM_REGS = 4;

type
  TRegisterIndex = 0..NUM_REGS - 1;
  TRegister = LongWord;

const
  RegNumberTable: array[TRegisterIndex] of TRegister = (
    $01000000,
    $01000001,
    $01000002,
    $01000003
  );

  RegStabsTable: array[TRegisterIndex] of ShortInt = (
    0, 1, 2, 3
  );

var
  I: TRegisterIndex;
begin
  for I := Low(TRegisterIndex) to High(TRegisterIndex) do
    WriteLn('Reg', I, ': $', HexStr(RegNumberTable[I], 8), ' stab=', RegStabsTable[I]);
end.
