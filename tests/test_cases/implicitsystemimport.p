{
  Test: Implicit System Unit Import
  
  FPC Behavior: System unit is automatically imported for all units.
  Types like SizeInt, PAnsiChar, etc. are available without explicit import.
  
  This test demonstrates that KGPC fails to compile units that use
  system types without explicitly importing the system unit.
  
  This is CRITICAL for FPC bootstrap because all RTL units depend on
  system types (strings.pp, sortbase.pp, etc.)
}
unit implicitsystemimport;

interface

type
  { These types are defined in system unit }
  TStringArray = array[0..10] of PAnsiChar;  { PAnsiChar from system }
  
function GetLength(s: PAnsiChar): SizeInt;  { SizeInt from system }

implementation

function GetLength(s: PAnsiChar): SizeInt;
var
  i: SizeInt;
begin
  i := 0;
  while s[i] <> #0 do
    Inc(i);
  GetLength := i;
end;

end.
