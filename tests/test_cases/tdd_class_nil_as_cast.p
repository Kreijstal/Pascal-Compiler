{ Regression test: (nil as T) must yield nil without dereferencing the
  source pointer.  KGPC previously emitted an unconditional VMT load for
  the runtime type check, which segfaulted when the source was nil.
  This was the third pp.pas bootstrap blocker — FPC's exception-handling
  CFI emitter casts a possibly-nil LSDA label via `hp.oper[0].sym as
  TAsmLabel` when generating CFI for a program that has no exception
  table data, so a nil source must round-trip to nil. }
program tdd_class_nil_as_cast;

{$mode objfpc}

type
  TBase = class
    procedure Touch; virtual;
  end;
  TDerived = class(TBase)
  end;

procedure TBase.Touch;
begin
end;

var
  b : TBase;
  d : TDerived;
begin
  b := nil;
  d := b as TDerived;
  if d = nil then
    Writeln('nil as Derived = nil OK')
  else
    Writeln('nil as Derived = nil FAIL');

  { Sanity: a real instance still casts and works. }
  b := TDerived.Create;
  d := b as TDerived;
  if d <> nil then
    Writeln('instance as Derived OK')
  else
    Writeln('instance as Derived FAIL');
  d.Free;
end.
