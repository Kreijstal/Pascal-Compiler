program ConditionalMacros;

{$define FANCY_FEATURE}
{$define LEGACY_MODE}

{$if defined(FANCY_FEATURE) and not defined(LEGACY_MODE)}
  this will cause a parse error if the conditional branch executes
{$else}
var
  Value: integer;
begin
  Value := 42;
  writeln(Value);
end.
{$endif}
