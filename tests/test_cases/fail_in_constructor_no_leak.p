{ Regression: `fail` inside a constructor is rewritten in-place to an Exit
  statement (semcheck_stmt_main).  The rewrite memset()s the statement's
  data union to switch it to exit_data, which previously clobbered the
  strdup'd procedure_call_data.id ("fail") and the other call-metadata
  strings without freeing them, leaking them on every constructor `fail`.

  The rewrite happens at semantic-check time whenever `fail` appears in a
  constructor body, so the leak triggers even when the statement is not
  reached at runtime.  Keep runtime output deterministic. }
program fail_in_constructor_no_leak;

type
  TObj = class
    v: Integer;
    constructor Create;
  end;

constructor TObj.Create;
begin
  v := 42;
  if v = 0 then
    fail;
end;

var
  a: TObj;
begin
  a := TObj.Create;
  writeln(a.v);
  a := nil;
end.
