unit complex_if_directive_unit;

{$if defined(FANCY_FEATURE) and not defined(LEGACY_MODE)}
  {$message 'This branch should be skipped'}
{$endif}

interface

implementation

end.
