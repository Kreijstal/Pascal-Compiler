program overloaded_method_shorter_arity_nonvirtual;
{$mode objfpc}

{ Regression: a class with two overloaded methods of the same name where the
  shorter-arity overload is NON-virtual and the longer-arity overload is
  VIRTUAL.  Calling the shorter overload must dispatch to the shorter method,
  not be misrouted to the virtual VMT slot of the longer overload (which would
  call the wrong method with a mismatched argument layout and leave the trailing
  parameters reading uninitialized stack).

  This mirrors FPC's ttgobj.GetLocal pair (a non-virtual 4-arg overload that
  forwards to a virtual 7-arg overload); the Win64 `on E:` exception codegen
  invokes the 4-arg form, and the misrouting corrupted the `def` argument and
  crashed the generated compiler. }

type
  TFoo = class
    { non-virtual short overload that forwards to the virtual long overload }
    procedure Bar(a: Integer; b: Integer);
    { virtual long overload }
    procedure Bar(a: Integer; b: Integer; c: Integer; d: Integer); virtual;
  end;

procedure TFoo.Bar(a: Integer; b: Integer);
begin
  { call to the 4-arg overload from inside the 2-arg overload }
  Bar(a, b, 30, 40);
end;

procedure TFoo.Bar(a: Integer; b: Integer; c: Integer; d: Integer);
begin
  WriteLn(a, ' ', b, ' ', c, ' ', d);
end;

var
  f: TFoo;
begin
  f := TFoo.Create;
  { calls the non-virtual 2-arg overload, which forwards to the 4-arg virtual }
  f.Bar(1, 2);
end.
