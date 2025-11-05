program TestInitializedVars;

{ Test various forms of initialized variable declarations }

var
  { Simple initialized variables }
  x: integer = 42;
  y: integer = 100;
  
  { Array with initializer }
  TestArray: array[0..2] of integer = (1, 2, 3);
  
  { Another array }
  Colors: array[1..3] of integer = (255, 128, 64);

begin
  WriteLn('x = ', x:1);
  WriteLn('y = ', y:1);
  WriteLn('TestArray[0] = ', TestArray[0]:1);
  WriteLn('TestArray[1] = ', TestArray[1]:1);
  WriteLn('TestArray[2] = ', TestArray[2]:1);
  WriteLn('Colors[1] = ', Colors[1]:1);
  WriteLn('Colors[2] = ', Colors[2]:1);
  WriteLn('Colors[3] = ', Colors[3]:1);
end.
