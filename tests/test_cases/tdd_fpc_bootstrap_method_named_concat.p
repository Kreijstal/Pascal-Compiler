{ TDD: method named Concat confused with built-in Concat in expression context.
  Bug: When a class method is named 'Concat' and called as an expression
  (x := obj.Concat(args)), the compiler intercepts it as the built-in string
  Concat function. Statement calls (obj.Concat(args);) work fine.
  Root cause: SemCheck_Expr_Access.c checks builtins before resolving method
  placeholders, and FindIdent("Concat") finds the builtin, not the method.
  Impact: ~541 errors in FPC compiler bootstrap (TLinkedList.Concat). }
program tdd_fpc_bootstrap_method_named_concat;

type
  TItem = class
    FValue: Integer;
    constructor Create(v: Integer);
    function Concat(other: TItem): TItem;
  end;

constructor TItem.Create(v: Integer);
begin
  FValue := v;
end;

function TItem.Concat(other: TItem): TItem;
begin
  Result := TItem.Create(FValue + other.FValue);
end;

var
  a, b, c: TItem;
begin
  a := TItem.Create(10);
  b := TItem.Create(20);

  { Statement context — works }
  a.Concat(b);

  { Expression context — this is the bug }
  c := a.Concat(b);
  WriteLn(c.FValue);
end.
