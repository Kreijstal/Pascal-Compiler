program tdd_pp_bootstrap_crt_pp_l20_repro;
{$mode fpc}

{ Originally reported as a pp_bootstrap miscompile on
  FPCSource/packages/rtl-console/src/unix/crt.pp line 20:
    {$i crth.inc}

  Investigation shows it is NOT a KGPC bug: pp_bootstrap reports
  "Compilation raised exception internally" at that location because
  it cannot find the include file 'crth.inc'.  crth.inc lives in
  FPCSource/packages/rtl-console/src/inc/, which must be on the
  include path (-I), not just the unit path (-Fu).  The baseline
  harness that produced the failure was missing the corresponding
  -I flag; once added, pp_bootstrap compiles crt.pp cleanly.

  FPC's error message is misleading there: an unfindable {$i ...}
  raises an internal exception rather than emitting a clear
  "Can't open include file" diagnostic.  That is FPC behavior, not
  a KGPC defect.

  As a positive guard, this test reproduces the kind of unit
  interface that crth.inc contributes to crt.pp (a var block that
  mixes typed-const-style initialized vars with plain
  uninitialized vars), and verifies KGPC compiles it cleanly. }

var
  LastMode: Word = 3;
  TextAttr: Byte = $07;
  WindMin:  Word = $0;
  WindMax:  Word = $184f;
  WindMinX: LongWord;
  WindMaxX: LongWord;

begin
  WindMinX := 0;
  WindMaxX := 1024;
  writeln(LastMode, ' ', TextAttr, ' ', WindMin, ' ', WindMax,
          ' ', WindMinX, ' ', WindMaxX);
end.
