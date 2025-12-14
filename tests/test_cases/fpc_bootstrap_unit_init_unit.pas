{ Test unit: initialization section with procedure call }
{ Pattern from FPC unix.pp: initialization section contains procedure calls }
unit fpc_bootstrap_unit_init_unit;

interface

var
  InitCount: Integer;

procedure ManualInit;

implementation

procedure ManualInit;
begin
  Inc(InitCount);
end;

initialization
  InitCount := 0;
  ManualInit;

end.
