{
  Minimal System Unit for FPC Bootstrap Compatibility
  
  Provides basic system types that FPC RTL units expect to be implicitly available.
}
unit system;

interface

type
  { Size types - platform dependent }
  SizeInt = LongInt;        { Signed size type }
  SizeUInt = LongWord;      { Unsigned size type }
  
  { Pointer types }
  PAnsiChar = ^Char;        { Pointer to ANSI character }
  PChar = ^Char;            { Alias for PAnsiChar }
  PPointer = ^Pointer;      { Pointer to pointer }
  
  { Additional common types }
  PByte = ^Byte;
  PWord = ^Word;
  PLongInt = ^LongInt;
  PLongWord = ^LongWord;
  PInteger = ^Integer;
  PCardinal = ^Cardinal;
  PShortInt = ^ShortInt;
  PSmallInt = ^SmallInt;

implementation

end.
