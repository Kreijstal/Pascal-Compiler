{ Mirrors FPC's rttih.inc: a {$MINENUMSIZE 1} directive (with trailing prose
  inside the directive braces) guarding an enum, all inside an included file.
  The enum's line number within this .inc is small, so a line-based directive
  scan that exits once the tracked line passes the target never sees the
  directive when the including file has more leading lines than that. }
{$PUSH}
{$MINENUMSIZE 1   this saves a lot of memory }
type
  TKind = (kA, kB, kC, kD);
{$MINENUMSIZE DEFAULT}
{$POP}
