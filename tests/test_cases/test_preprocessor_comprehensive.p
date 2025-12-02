program TestPreprocessorComprehensive;
{ Comprehensive test of FPC preprocessor features - Chapter 2 }
{ This test file validates behavior against FPC }

{$define FLAG_A}
{$define FLAG_B}
{$macro on}
{$define VERSION := 30400}

begin
  { Test 1: Basic $IFDEF }
  {$ifdef FLAG_A}
  WriteLn('1: FLAG_A defined');
  {$endif}

  { Test 2: $IFDEF with undefined }
  {$ifdef UNDEFINED_FLAG}
  WriteLn('ERROR: should not print');
  {$else}
  WriteLn('2: UNDEFINED_FLAG not defined');
  {$endif}

  { Test 3: $IFNDEF }
  {$ifndef NONEXISTENT}
  WriteLn('3: NONEXISTENT does not exist');
  {$endif}

  { Test 4: $IF with DEFINED() }
  {$if defined(FLAG_A)}
  WriteLn('4: FLAG_A via DEFINED()');
  {$endif}

  { Test 5: NOT DEFINED() }
  {$if not defined(NONEXISTENT)}
  WriteLn('5: NOT DEFINED works');
  {$endif}

  { Test 6: Boolean AND }
  {$if defined(FLAG_A) and defined(FLAG_B)}
  WriteLn('6: FLAG_A AND FLAG_B');
  {$endif}

  { Test 7: Boolean OR }
  {$if defined(FLAG_A) or defined(NONEXISTENT)}
  WriteLn('7: FLAG_A OR NONEXISTENT');
  {$endif}

  { Test 8: Macro value comparison > }
  {$if VERSION > 30000}
  WriteLn('8: VERSION > 30000');
  {$endif}

  { Test 9: Macro value comparison = }
  {$if VERSION = 30400}
  WriteLn('9: VERSION = 30400');
  {$endif}

  { Test 10: Macro value comparison < }
  {$if VERSION < 40000}
  WriteLn('10: VERSION < 40000');
  {$endif}

  { Test 11: Numeric $IF 1 }
  {$if 1}
  WriteLn('11: $if 1 is true');
  {$endif}

  { Test 12: Numeric $IF 0 }
  {$if 0}
  WriteLn('ERROR: should not print');
  {$else}
  WriteLn('12: $if 0 is false');
  {$endif}

  { Test 13: Arithmetic + }
  {$if 10+5=15}
  WriteLn('13: 10+5=15');
  {$endif}

  { Test 14: Arithmetic - }
  {$if 10-5=5}
  WriteLn('14: 10-5=5');
  {$endif}

  { Test 15: Arithmetic * }
  {$if 10*5=50}
  WriteLn('15: 10*5=50');
  {$endif}

  { Test 16: DIV }
  {$if 10 div 5=2}
  WriteLn('16: 10 div 5=2');
  {$endif}

  { Test 17: MOD }
  {$if 10 mod 3=1}
  WriteLn('17: 10 mod 3=1');
  {$endif}

  { Test 18: $ELSEIF }
  {$if defined(NONEXISTENT1)}
  WriteLn('ERROR');
  {$elseif defined(FLAG_A)}
  WriteLn('18: $ELSEIF works');
  {$endif}

  { Test 19: $UNDEF }
  {$define TEMP_FLAG}
  {$undef TEMP_FLAG}
  {$ifndef TEMP_FLAG}
  WriteLn('19: $UNDEF works');
  {$endif}

  { Test 20: Nested conditionals }
  {$ifdef FLAG_A}
    {$ifdef FLAG_B}
  WriteLn('20: Nested conditionals work');
    {$endif}
  {$endif}

  { Test 21: $IFEND as alias for $ENDIF }
  {$if defined(FLAG_A)}
  WriteLn('21: $IFEND works');
  {$ifend}

  { Test 22: >= comparison }
  {$if VERSION >= 30400}
  WriteLn('22: VERSION >= 30400');
  {$endif}

  { Test 23: <= comparison }
  {$if VERSION <= 30400}
  WriteLn('23: VERSION <= 30400');
  {$endif}

  { Test 24: <> comparison }
  {$if VERSION <> 99999}
  WriteLn('24: VERSION <> 99999');
  {$endif}

  { Test 25: Complex boolean }
  {$if (defined(FLAG_A) or defined(NONEXISTENT)) and defined(FLAG_B)}
  WriteLn('25: Complex boolean expression');
  {$endif}

  WriteLn('All tests completed');
end.
