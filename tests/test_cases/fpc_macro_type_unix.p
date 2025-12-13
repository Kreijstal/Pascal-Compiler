{$MACRO ON}
program fpc_macro_type_unix;
{ Test that platform symbol 'Unix' is NOT expanded in type names }
{ When {$MACRO ON} is enabled, platform conditionals like 'Unix' }
{ should remain as identifiers - not expanded as macros. }
{ This is critical for FPC bootstrap where Unit Unix exists. }

type
  { Type named Unix - should NOT be expanded to '1' }
  Unix = Integer;

var
  x: Unix;

begin
  x := 42;
  WriteLn(x);
end.
