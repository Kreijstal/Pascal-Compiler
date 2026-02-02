program sizeint_capsizeint_directive;
begin
  {$if SizeOf(SizeInt) > SizeOf(Integer)}
  writeln('capsizeint');
  {$else}
  writeln('nocapsizeint');
  {$endif}
end.
