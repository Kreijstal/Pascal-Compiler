unit file_type_unit_test;

interface

type
  TIntegerFile  = file of Integer;
  TExtendedFile = file of Real;
  TCharFile     = file of Char;

var
  FInt: TIntegerFile;
  FChr: TCharFile;

implementation

end.
