program reg_in_constset_identifier_codegen;

type
  TNodeType = (
    n0, n1, n2, n3, n4, n5, n6, n7, n8, n9,
    n10, n11, n12, n13, n14, n15, n16, n17, n18, n19,
    n20, n21, n22, n23, n24, n25, n26, n27, n28, n29,
    n30, n31
  );

function Matches(x: TNodeType; b: Boolean): Boolean;
const
  RationalOperators = [n1, n2, n3, n4, n5, n21];
begin
  if b then
    Matches := x in RationalOperators
  else
    Matches := False;
end;

begin
  if Matches(n1, True) then
    writeln('hit')
  else
    writeln('miss');

  if Matches(n7, True) then
    writeln('hit')
  else
    writeln('miss');

  if Matches(n1, False) then
    writeln('hit')
  else
    writeln('miss');
end.
