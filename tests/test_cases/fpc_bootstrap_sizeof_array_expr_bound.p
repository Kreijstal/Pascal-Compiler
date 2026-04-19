program fpc_bootstrap_sizeof_array_expr_bound;

type
  TDynamicBlockData = array[0..1024 * 1024 - 1] of Byte;
  PDynamicBlock = ^TDynamicBlock;
  TDynamicBlock = record
    Pos: LongWord;
    Size: LongWord;
    Used: LongWord;
    Next: PDynamicBlock;
    Data: TDynamicBlockData;
  end;

const
  DynamicBlockBaseSize = SizeOf(TDynamicBlock) - SizeOf(TDynamicBlockData);

begin
  WriteLn(DynamicBlockBaseSize);
end.
