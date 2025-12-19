{
  Minimal System Unit for FPC Bootstrap Compatibility
  
  Provides basic system types that FPC RTL units expect to be implicitly available.
}
unit system;

interface

{ NOTE: KGPC merges `KGPC/stdlib.p` (the implicit prelude) into all compilations.
  System-level compatibility types like `SizeInt`, pointer aliases, and `ShortString`
  are defined there to avoid duplicate declarations when users also `uses system`. }

implementation

end.
