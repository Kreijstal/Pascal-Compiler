{$mode objfpc}
program tdd_large_set_intersection_eq;

type
  TBigEnum = (
    e0, e1, e2, e3, e4, e5, e6, e7, e8, e9,
    e10, e11, e12, e13, e14, e15, e16, e17, e18, e19,
    e20, e21, e22, e23, e24, e25, e26, e27, e28, e29,
    e30, e31, e32, e33, e34, e35, e36, e37, e38, e39,
    e40, e41, e42, e43, e44, e45, e46, e47, e48, e49,
    e50, e51, e52, e53, e54, e55, e56, e57, e58, e59
  );
  TBigSet = set of TBigEnum;
  TProcdef = class
    procoptions: TBigSet;
  end;
  PCandidate = ^TCandidate;
  TCandidate = record
    data: TProcdef;
    next: PCandidate;
  end;

procedure TestCheck(hp: PCandidate);
begin
  if (hp^.data.procoptions * [e32, e45] = []) then
    writeln('none')
  else
    writeln('has');
end;

var
  c: TCandidate;
  pd: TProcdef;
begin
  pd := TProcdef.Create;
  c.data := pd;
  c.next := nil;

  pd.procoptions := [e10, e20];
  TestCheck(@c);
  pd.procoptions := [e10, e32];
  TestCheck(@c);
  pd.procoptions := [e45];
  TestCheck(@c);

  pd.Free;
end.
