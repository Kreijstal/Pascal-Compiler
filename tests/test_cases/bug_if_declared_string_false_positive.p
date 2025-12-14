{ Test: declared() must ignore string literals that look like declarations }
program bug_if_declared_string_false_positive;
{$mode objfpc}

begin
  WriteLn('var Phantom: Integer;');
  {$if declared(Phantom)}
  WriteLn('BUG: declared() matched a string literal');
  {$else}
  WriteLn('OK');
  {$endif}
end.
