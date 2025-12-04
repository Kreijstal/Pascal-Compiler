program hex_constants;
{ Test hex, binary, and octal integer literals }
const
  HexA = $FF;
  HexB = $FFFF;
  HexC = $FFFFFFFF;
  HexD = $12AB;
  
begin
  writeln(HexA);
  writeln(HexB);
  writeln(HexC);
  writeln(HexD);
end.
