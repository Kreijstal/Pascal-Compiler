{ Regression test: passing a pshortstring (pointer-to-shortstring) variable
  to a VAR parameter of type pshortstring must pass the ADDRESS of the
  local variable, not its VALUE.  A previous KGPC codegen bug treated
  pshortstring as a shortstring (because alias->target_type_id='shortstring')
  and loaded the pointer value instead of taking its address. }
program regr_varparam_pshortstring;

type
  pshortstring = ^shortstring;

procedure clear_ptr(var p: pshortstring);
begin
  p := nil;
end;

var
  s: shortstring;
  ps: pshortstring;
begin
  s := 'hello';
  ps := @s;
  writeln('before: ps=', ps <> nil);
  clear_ptr(ps);
  writeln('after: ps=', ps = nil);
  if ps = nil then
    writeln('OK')
  else
    writeln('BUG: ps should be nil');
end.
