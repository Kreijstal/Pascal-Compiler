unit tdd_pp_bootstrap_transitive_uses_shadowing_middle;

{$mode objfpc}

interface

uses tdd_pp_bootstrap_transitive_uses_shadowing_ptrunit;

{ This unit transitively pulls in the pointer-typed TFoo declaration but
  does not re-export it. Consumers that uses *_intunit directly must still
  see the qword-typed TFoo. }

procedure dummy_middle;

implementation

procedure dummy_middle;
begin
end;

end.
