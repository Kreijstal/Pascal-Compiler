program fpc_bootstrap_new_multidim_array_pointer;

type
  TOpcode = (op0, op1);
const
  MaxOperands = 100000;
type
  TTable = array[TOpcode, 0..MaxOperands] of byte;
  PTable = ^TTable;

var
  table: PTable;

begin
  New(table);
  FillChar(table^, SizeOf(TTable), 0);
  table^[op1, MaxOperands] := 170;
  WriteLn(SizeOf(TTable));
  WriteLn(table^[op1, MaxOperands]);
  Dispose(table);
end.
