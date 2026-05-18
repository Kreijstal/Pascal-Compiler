program nested_result_from_classref_constructor;
{$mode objfpc}

{ Regression test: a function whose Result is assigned, inside a nested
  procedure, from `<classref>.<constructor>(...)` with deeply chained
  constructor parameters.  KGPC's codegen for the chained constructor
  args can end up freeing the outer call's target register through the
  arg_eval spill path in codegen_pass_arguments, so the subsequent
  static-link load in codegen_var_assignment (scope_depth > 0) is
  allowed to reallocate the same physical register and clobber the
  return value before the final store.  Mirrors the structural pattern
  of nadd.pas first_addset:call_varset_helper where
    result := ccallnode.createintern(name,
                ccallparanode.create(... 4 levels ...));
  causes a SIGSEGV in pp_bootstrap.  Fix: spill the RHS value to a
  stack slot before acquiring the static link, reload after. }

type
  TNode = class
    n : TNode;
    v : longint;
    constructor Create(_v : longint; nxt : TNode);
  end;
  TList = class
    head : TNode;
    constructor CreateFromChain(name : shortstring; chain : TNode);
  end;
  TListClass = class of TList;
  TNodeClass = class of TNode;

constructor TNode.Create(_v : longint; nxt : TNode);
begin v := _v; n := nxt; end;
constructor TList.CreateFromChain(name : shortstring; chain : TNode);
begin head := chain; end;

var
  gList : TListClass = TList;
  gNode : TNodeClass = TNode;

function outer : TList;
  procedure helper(const nm : shortstring);
  begin
    outer := gList.CreateFromChain(nm,
      gNode.Create(1,
      gNode.Create(2,
      gNode.Create(3,
      gNode.Create(4,
      gNode.Create(5,
      gNode.Create(6, nil)))))));
  end;
begin
  outer := nil;
  helper('hi');
end;

var
  r : TList; cur : TNode;
begin
  r := outer;
  if r = nil then
  begin
    writeln('FAIL: outer returned nil');
    halt(1);
  end;
  cur := r.head;
  while cur <> nil do
  begin
    write(cur.v,' ');
    cur := cur.n;
  end;
  writeln;
end.
