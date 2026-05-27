unit tdd_pp_bootstrap_transitive_uses_shadowing_consumer;

{$mode objfpc}

interface

{ Direct uses: intunit (TFoo = qword).
  Transitive uses: middle -> ptrunit (TFoo = ^integer).
  TFoo declared inside this unit must resolve to qword via the direct
  dependency; if it resolves to the transitive pointer alias, inc() below
  fails semcheck. }

uses
  tdd_pp_bootstrap_transitive_uses_shadowing_intunit,
  tdd_pp_bootstrap_transitive_uses_shadowing_middle;

procedure run_test;

implementation

procedure run_test;
var
  v: TFoo;
begin
  v := 41;
  inc(v, 1);
  writeln(v);
end;

end.
