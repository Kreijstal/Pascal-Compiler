program tdd_minenumsize_in_include;
{$mode objfpc}
{ Padding so the include directive below sits at a line number greater than
  the enum's line number inside the .inc. This is what trips a line-based
  directive scan: it stops at this file's matching line before ever reaching
  the included directive. }
{ pad }
{ pad }
{ pad }
{ pad }
{ pad }
{ pad }
{$i tdd_minenumsize_in_include.inc}
type
  TKindRec = packed record
    tag: TKind;
    rest: array[0..2] of LongInt;
  end;
begin
  Writeln(SizeOf(TKind));
  Writeln(SizeOf(TKindRec));
  Writeln(Ord(High(TKind)));
end.
