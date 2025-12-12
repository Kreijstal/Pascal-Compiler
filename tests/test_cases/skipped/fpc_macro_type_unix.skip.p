{$MACRO ON}
program fpc_macro_type_unix;
{ SKIPPED: KGPC expands platform defines in identifier positions }
{ }
{ Test that platform symbol 'Unix' is NOT expanded in type names }
{ When {$MACRO ON} is enabled, 'Unix' should remain 'Unix' }
{ not become '1' through macro expansion. }
{ }
{ This is critical for FPC bootstrap where Unit Unix exists. }
{ }
{ BUG: KGPC expands "Unix" to "1" in all identifier contexts }
{ resulting in: type 1 = Integer; which compiles but produces }
{ no output instead of "42" }
{ }
{ This compiles and runs correctly with FPC, outputting: }
{   42 }

type
  { Type named Unix - the name must NOT be expanded to '1' }
  Unix = Integer;

var
  x: Unix;

begin
  x := 42;
  WriteLn(x);
end.
