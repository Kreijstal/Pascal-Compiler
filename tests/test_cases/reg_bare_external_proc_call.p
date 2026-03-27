program reg_bare_external_proc_call;

procedure kgpc_prefetch(p: pointer); external;

var
  value: LongInt;
begin
  value := 42;
  kgpc_prefetch(@value);
  WriteLn(value);
end.
