program FPCConstTypeCast;
{
  Test const declarations with type casting/conversion.
  This pattern is used in FPC RTL's charset.pp:
    const
      UNKNOW_CHAR_A = ansichar(63);
      UNKNOW_CHAR_W = tunicodechar(63);
}

const
  { Type cast from integer to char in const declaration }
  QUESTION_MARK = char(63);  { ASCII '?' }
  SPACE_CHAR = char(32);     { ASCII ' ' }
  
  { Type cast from integer to byte }
  MAX_BYTE = byte(255);
  HALF_BYTE = byte(128);

var
  c: char;
  b: byte;

begin
  { Use the const values }
  c := QUESTION_MARK;
  WriteLn('Question mark: ', c);
  
  c := SPACE_CHAR;
  WriteLn('Space char: [', c, ']');
  
  b := MAX_BYTE;
  WriteLn('Max byte: ', b);
  
  b := HALF_BYTE;
  WriteLn('Half byte: ', b);
  
  WriteLn('Test completed');
end.
