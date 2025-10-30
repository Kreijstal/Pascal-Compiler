program ConditionalMacrosInvalidSyntax;

{$if defined(MISSING_SYMBOL}
begin
  writeln('invalid');
end.
