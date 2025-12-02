{ Minimal test case for parser bug with type Double and keyword-containing comments
  This test passes in FPC but fails in KGPC parser.
  
  Bug 1: "TReal = type Double" is misparsed - 'type' is treated as a type identifier
  Bug 2: Comments containing 'implementation' keyword trigger IMPLEMENTATION_SECTION
  Bug 3: Comments containing 'initialization' keyword trigger INITIALIZATION_SECTION
}
unit unitwithimplcomment;

interface

type
  { Having a record with dword fields seems to be part of the trigger }
  TFloat64 = record
    lo, hi: dword;
  end;

type
  { This line fails: 'type' is parsed as the type identifier, not as the distinct-type keyword }
  TReal = type Double;
  TValReal = Extended;

const
  X = 1;

type
  DWord = LongWord;
{ This comment contains the implementation keyword which confuses the parser }
  Cardinal = LongWord;

const
  Y = 2;
(* This comment contains initialization which also confuses the parser *)

implementation

end.
