{
  Minimal System Unit for FPC Bootstrap Compatibility
  
  Provides basic system types that FPC RTL units expect to be implicitly available.
}
unit system;

interface

type
  { ShortString: length-prefixed string[255] compatible layout.
    Note: most bootstrap-compatible aliases live in KGPC/stdlib.p (the implicit prelude). }
  ShortString = array[0..255] of Char;

implementation

end.
