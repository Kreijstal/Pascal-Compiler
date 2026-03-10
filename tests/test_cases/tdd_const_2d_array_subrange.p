{ TDD: 2D const array indexed by subranges
  Pattern from FPC compiler: convertopsse : array[OS_F32..OS_F128,OS_F32..OS_F128] of tasmop
  Error: "element 0 initializer count 5 does not match declared range 0..0" }
program tdd_const_2d_array_subrange;

type
  TOpSize = (OS_NO, OS_8, OS_16, OS_32, OS_F32, OS_F64, OS_F80, OS_F128);
  TAsmOp = (A_NONE, A_MOVSS, A_MOVSD, A_CVTSS2SD, A_CVTSD2SS, A_MOVAPS);

const
  ConvertOp: array[OS_F32..OS_F128, OS_F32..OS_F128] of TAsmOp = (
    (A_MOVSS, A_CVTSS2SD, A_NONE, A_NONE),
    (A_CVTSD2SS, A_MOVSD, A_NONE, A_NONE),
    (A_NONE, A_NONE, A_NONE, A_NONE),
    (A_NONE, A_NONE, A_NONE, A_MOVAPS)
  );

var
  I, J: TOpSize;
begin
  for I := OS_F32 to OS_F128 do
  begin
    for J := OS_F32 to OS_F128 do
      Write(Ord(ConvertOp[I, J]), ' ');
    WriteLn;
  end;
end.
