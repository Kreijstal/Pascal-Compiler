program ConditionalMacrosUndefined;

begin
{$if MISSING_SYMBOL}
  writeln('feature enabled');
{$endif}
  writeln('done');
end.
