{ Test System.Close and System.Assign qualified procedure calls }
{ Required for FPC bootstrap - objpas.pp uses System.Assign, System.Close }
{ These are unit-qualified procedure calls where the unit is System }
program fpc_bootstrap_system_qualified_proccall;

var
  t: Text;
begin
  { Use a repo-local path so Windows runs do not depend on /tmp existing }
  System.Assign(t, 'tests/output/system_assign_test.txt');
  Rewrite(t);
  WriteLn(t, 'Hello from System.Assign');
  System.Close(t);
  WriteLn('OK');
end.
