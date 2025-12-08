{
  Test: Const Expression with Function Calls
  
  FPC Behavior: Supports Chr() and Ord() in const expressions beyond
  what KGPC currently supports.
  
  This test demonstrates const expression features needed by FPC RTL units.
  
  This is CRITICAL for FPC bootstrap because charset.pp and other RTL units
  use Chr() in const initialization.
}
program ConstExprFunctions;

const
  { Simple const expressions that KGPC supports }
  MaxByte = High(Byte);           { KGPC supports this }
  CharSize = SizeOf(Char);        { KGPC supports this }
  ZeroOrd = Ord('0');             { KGPC supports this }
  
  { Chr() in const expressions - FPC supports, KGPC might not }
  CharA = Chr(65);                { FPC supports, needed for charset.pp }
  CharZ = Chr(90);                { FPC supports, needed for charset.pp }
  NullChar = Chr(0);              { FPC supports, needed for RTL }
  
  { Arithmetic in const expressions }
  CalcValue = 10 + 20;            { Both support }
  ArraySize = MaxByte + 1;        { Both should support }

begin
  WriteLn('MaxByte: ', MaxByte);
  WriteLn('CharSize: ', CharSize);
  WriteLn('CharA: ', CharA);
  WriteLn('CharZ: ', CharZ);
  WriteLn('CalcValue: ', CalcValue);
  WriteLn('ArraySize: ', ArraySize);
end.
