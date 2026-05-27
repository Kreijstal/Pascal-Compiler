{ Regression: FPC's rtl/win/sysutils.pp WinAPICompareFlags is a typed
  const declared as

      WinAPICompareFlags : array [TCompareOption] of LongWord
        = ({...comments hiding all but one element...} NORM_IGNORECASE {...});

  i.e. an array initializer whose lone uncommented element is a bare
  constant identifier.  The parser flattens `(IDENT)` to a single
  PASCAL_T_IDENTIFIER node, and the typed-const lowering used to reject
  that for non-char-array targets ("Const array X string initializer
  requires a char array type").  It now wraps the lone identifier in a
  synthetic 1-element tuple so the array gets a real element-0 value. }
{$mode objfpc}
program ConstArraySingleIdentifierInit;

type
  TOption = (opt_only);

const
  Flag_A: LongWord = 42;
  Flag_B: LongInt  = -7;
  Flags_A: array [TOption] of LongWord = (Flag_A);
  Flags_B: array [TOption] of LongInt  = (Flag_B);

begin
  Writeln(Flags_A[opt_only]);
  Writeln(Flags_B[opt_only]);
end.
