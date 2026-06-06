program tdd_methodptr_param_call;
{ Regression: calling a method pointer ("procedure of object") that is held in
  a plain variable or parameter must pass the hidden Self/data half of the
  TMethod as the implicit first argument.

  KGPC's plain-variable procedural-call path set is_procedural_var_call but
  never built a procedural_var_expr, so codegen could not locate the TMethod
  storage to load Self from.  It fell back to a bare indirect call that dropped
  Self and shifted every explicit argument by one register.  do_replace_node_regs
  in FPC's hlcgobj.pas is invoked exactly this way (via foreachnode's adapter),
  so the KGPC-bootstrapped FPC 3.2.2 compiler crashed during proc-body codegen
  (thlcgobj.do_replace_node_regs reading n.flags off a shifted argument).

  Mirrors that shape: a method pointer passed as a parameter and called
  directly, plus the same through a record field and a local variable. }
{$mode objfpc}{$H-}
type
  tnode = class
    val: longint;
  end;
  fenres = (r_false, r_true);
  methodfn = function(var n: tnode; arg: pointer): fenres of object;

  TWorker = class
    tag: longint;
    function dorepl(var n: tnode; arg: pointer): fenres;
  end;

var
  marker: longint;

function TWorker.dorepl(var n: tnode; arg: pointer): fenres;
begin
  { Self.tag, n and arg must all be correct -> Self was passed and args
    were not shifted. }
  if (tag = 7) and (n.val = 99) and (arg = @marker) then
    writeln('ok ', n.val)
  else
    writeln('BAD tag=', tag, ' nval=', n.val, ' argok=', ord(arg = @marker));
  result := r_true;
end;

{ direct call of a method-pointer PARAMETER }
function callit(var n: tnode; f: methodfn; arg: pointer): fenres;
begin
  result := f(n, arg);
end;

var
  w: TWorker;
  root: tnode;
  mp: methodfn;
begin
  marker := 12345;
  w := TWorker.Create;
  w.tag := 7;
  root := tnode.Create;
  root.val := 99;
  callit(root, @w.dorepl, @marker);   { method ptr via parameter }
  mp := @w.dorepl;
  mp(root, @marker);                  { method ptr via local variable }
  root.Free;
  w.Free;
end.
