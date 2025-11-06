program enum_writeln;
type
  beverage = (coffee, tea, milk, water, coke, limejuice);
var
  drink: beverage;
begin
  drink := limejuice;
  writeln(drink);
end.
