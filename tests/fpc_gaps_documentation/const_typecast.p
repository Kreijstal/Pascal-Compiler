{
  Test: Type casting in const expressions
  
  FPC Behavior: Supports type casting like Char(65) or Word(1000) in const expressions
  Example: const UNKNOW_CHAR = Char(63);
  
  This is used in charset.pp for character constants.
  
  CRITICAL for FPC bootstrap: charset.pp uses this pattern.
  
  STATUS: ❌ FAILS with KGPC - type casting in const expressions not supported
}
program ConstTypeCast;

const
  CharFromInt = Char(65);          { Type cast integer to char }
  WordValue = Word(1000);          { Type cast to word }
  ByteValue = Byte(255);           { Type cast to byte }
  
begin
  WriteLn('CharFromInt: ', CharFromInt);
  WriteLn('WordValue: ', WordValue);
  WriteLn('ByteValue: ', ByteValue);
  WriteLn('Const typecast test passed');
end.
