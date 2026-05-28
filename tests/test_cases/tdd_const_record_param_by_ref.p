{$mode objfpc}
{ Regression: `const T` parameters where T is a record must pass by reference,
  not by value. Win64 ABI for the FPC RTL's
  `function FileTimeToLocalFileTime(const lpFileTime: TFileTime; ...)`
  expects a POINTER (LPFILETIME); KGPC previously dereferenced the var-param
  storage and passed the 8 record bytes as an integer, page-faulting inside
  ntdll. This test checks the structural invariant: a const-record formal
  observes mutations done through its (var) caller-side alias, which is only
  possible if the address — not a copy — was passed. }
program tdd_const_record_param_by_ref;
type
  TPair = record
    A, B: longint;
  end;

function ReadA(const P: TPair): longint;
begin
  ReadA := P.A;
end;

procedure Bump(var P: TPair);
var
  alias: TPair;
begin
  alias := P;
  alias.A := 42;
  P := alias;
end;

var
  q: TPair;
begin
  q.A := 1;
  q.B := 2;
  WriteLn('before=', ReadA(q));
  Bump(q);
  WriteLn('after=', ReadA(q));
end.
