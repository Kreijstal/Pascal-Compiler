program TestOrd;
type
  TColors = (cRed, cGreen, cBlue);
var
  Color: TColors;
  ColorOrd: integer;
begin
  Color := cGreen;
  ColorOrd := ord(Color);
  writeln('The order of cGreen is: ', ColorOrd);
end.
