program tdd_sizeof_field_in_with;
{ Regression: sizeof(field_name) inside `with record_var do ...`
  must return the field's size, not the enclosing record's.
  Triggers KGPC's WITH-context sizeof fallback path in
  semcheck_builtin_sizeof. }

type
  TSwitch = (sw_a, sw_b, sw_c);
  TSwitches = set of TSwitch;
  TFoo = record
    aligns: array[1..16] of LongInt;
    switches: TSwitches;
    pmessage: pointer;
  end;

var
  foo: TFoo;
  sz_in_with: longint;

begin
  with foo do
    sz_in_with := sizeof(switches);

  if sz_in_with <> sizeof(foo.switches) then
  begin
    writeln('FAIL: sizeof(switches in with)=', sz_in_with,
            ' but sizeof(foo.switches)=', sizeof(foo.switches));
    halt(1);
  end;
  writeln('OK');
end.
