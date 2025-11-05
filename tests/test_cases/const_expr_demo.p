program ConstExprDemo;

const
  Greeting       = 'Hello, ' + 'world!';
  Radius         = 3.0;
  TwoPi          = 2.0 * 3.1415926535897932385;
  Circumference  = TwoPi * Radius;   { const expression referencing other consts }
  TwiceSeven     = 2 * 7;

begin
  WriteLn(Greeting);
  WriteLn('TwiceSeven = ', TwiceSeven);
  WriteLn('Circumference for r=', Radius:0:1, ' is ', Circumference:0:3);
end.
