program fpc_bootstrap_currency_type;
{
  Regression: Currency must be resolvable in FPC RTL mode (--no-stdlib).
  This is required for system.pp, sstrings.inc, text.inc, and variant.inc.
}

var
  c: Currency;
begin
  c := 0.0;
  if c = 0.0 then
    writeln('OK')
  else
    writeln('FAIL');
end.
