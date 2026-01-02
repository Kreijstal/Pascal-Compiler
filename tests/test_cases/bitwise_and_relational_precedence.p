{ Test Pascal operator precedence: and/or have higher precedence than =/<> }
program bitwise_and_relational_precedence;

const
  faDirectory = 16;
  faReadOnly = 1;
  
var
  Attr: LongInt;
  passed: boolean;
  
begin
  passed := true;
  
  { Test 1: Bitwise AND with equality - should parse as (Attr and faDirectory) = faDirectory }
  Attr := 16;
  if Attr and faDirectory = faDirectory then
    writeln('Test 1 PASS: Attr and faDirectory = faDirectory')
  else
  begin
    writeln('Test 1 FAIL: Expected true for Attr and faDirectory = faDirectory');
    passed := false;
  end;
  
  { Test 2: Same with different value }
  Attr := 17;  { has both faReadOnly and faDirectory bits }
  if Attr and faDirectory = faDirectory then
    writeln('Test 2 PASS: Combined flags detected correctly')
  else
  begin
    writeln('Test 2 FAIL: Should detect faDirectory in combined flags');
    passed := false;
  end;
  
  { Test 3: Negative case }
  Attr := 1;  { only faReadOnly }
  if Attr and faDirectory = faDirectory then
  begin
    writeln('Test 3 FAIL: Should not detect faDirectory');
    passed := false;
  end
  else
    writeln('Test 3 PASS: Correctly rejected non-directory');
  
  if passed then
    writeln('ALL TESTS PASSED')
  else
    writeln('SOME TESTS FAILED');
end.
