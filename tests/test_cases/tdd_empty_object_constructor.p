{ A fieldless TP `object` is zero bytes; its constructor must still compile.
  Regression for the FPC compiler's `TCondRegs = object` (aoptcpub.pas), which
  previously failed codegen with "Unable to determine size for record return
  value of TCondRegs__Init". }
program tdd_empty_object_constructor;
type
  TCondRegs = object
    constructor Init;
    destructor Done;
  end;
constructor TCondRegs.Init;
begin
end;
destructor TCondRegs.Done;
begin
end;
var
  c: TCondRegs;
begin
  c.Init;
  c.Done;
  writeln('ok');
end.
