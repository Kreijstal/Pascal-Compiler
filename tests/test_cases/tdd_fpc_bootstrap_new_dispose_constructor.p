{ TDD: Turbo Pascal-style New(ptr, Constructor) and Dispose(ptr, Destructor).
  Bug: KGPC only accepts New(p) and Dispose(p), rejecting the two-argument
  form that allocates memory AND calls a constructor/destructor.
  Used in FPC compiler for object (not class) types. }
program tdd_fpc_bootstrap_new_dispose_constructor;

type
  PNode = ^TNode;
  TNode = object
    value: Integer;
    next: PNode;
    constructor Create(v: Integer);
    destructor Destroy; virtual;
  end;

constructor TNode.Create(v: Integer);
begin
  value := v;
  next := nil;
end;

destructor TNode.Destroy;
begin
  { cleanup }
end;

var
  p1, p2: PNode;
begin
  { Two-argument New: allocate + call constructor }
  New(p1, Create(42));
  New(p2, Create(99));
  p1^.next := p2;

  WriteLn(p1^.value);
  WriteLn(p1^.next^.value);

  { Two-argument Dispose: call destructor + free }
  Dispose(p2, Destroy);
  Dispose(p1, Destroy);
end.
