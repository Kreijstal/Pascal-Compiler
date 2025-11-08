program array_neg_index_const;
{* 
  Test array with negative lower bound and constant upper bound.
  This tests that arrays like array[-1..N] where N is a const are
  correctly handled and not treated as dynamic arrays.
*}
const
  MaximalList = 10;
var
  SymbolNameList: array[-1..MaximalList] of integer;
  i: integer;
begin
  { Initialize array elements }
  SymbolNameList[-1] := -1;
  SymbolNameList[0] := 0;
  SymbolNameList[1] := 1;
  SymbolNameList[10] := 10;
  
  { Verify values }
  if SymbolNameList[-1] <> -1 then
    writeln('FAIL: SymbolNameList[-1] = ', SymbolNameList[-1], ' expected -1')
  else if SymbolNameList[0] <> 0 then
    writeln('FAIL: SymbolNameList[0] = ', SymbolNameList[0], ' expected 0')
  else if SymbolNameList[1] <> 1 then
    writeln('FAIL: SymbolNameList[1] = ', SymbolNameList[1], ' expected 1')
  else if SymbolNameList[10] <> 10 then
    writeln('FAIL: SymbolNameList[10] = ', SymbolNameList[10], ' expected 10')
  else
    writeln('PASS');
end.
