{ Test System.Close and System.Assign qualified procedure calls }
{ Required for FPC bootstrap - objpas.pp uses System.Assign, System.Close }
{ These are unit-qualified procedure calls where the unit is System }
program fpc_bootstrap_system_qualified_proccall;

var
  t: Text;
begin
  System.Assign(t, '/tmp/test_output.txt');
  Rewrite(t);
  WriteLn(t, 'Hello from System.Assign');
  System.Close(t);
  WriteLn('OK');
end.
