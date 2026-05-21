program tdd_with_method_receiver;
{ Regression test for: with X do <method>; binding to enclosing Self.
  When `Hit` is a method on both TUser (the enclosing class) and TFoo
  (the with-target's class), an unqualified call inside `with f do`
  must resolve to f.Hit, not Self.Hit. }
type
  TFoo = class
    Tag: longint;
    procedure Hit;
  end;

  TUser = class
    Tag: longint;
    procedure Hit;
    procedure DoIt;
  end;

procedure TFoo.Hit;
begin
  writeln('Foo.Hit ', Tag);
end;

procedure TUser.Hit;
begin
  writeln('User.Hit ', Tag);
end;

procedure TUser.DoIt;
var f: TFoo;
begin
  Tag := 1;
  f := TFoo.Create;
  f.Tag := 42;
  with f do
    Hit;            { should call f.Hit (Foo.Hit 42), not Self.Hit (User.Hit 1) }
  f.Free;
end;

var u: TUser;
begin
  u := TUser.Create;
  u.DoIt;
  writeln('OK');
end.
