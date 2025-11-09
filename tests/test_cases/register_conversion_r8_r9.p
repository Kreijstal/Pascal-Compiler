program RegisterConversionTest;
{ Test to ensure r8d/r9d registers are handled correctly in division operations }
var
  a, b, c: integer;
begin
  { These operations should generate code that uses r8d and r9d }
  { and test the register conversion functions }
  a := 255;
  b := a mod 256;  { Forces division/modulo which may use r8d }
  c := b div 2;    { Another division }
  writeln(b);
  writeln(c);
  
  { More division operations to exercise register allocation }
  a := 1000;
  b := a div 100;
  c := a mod 100;
  writeln(b);
  writeln(c);
end.
