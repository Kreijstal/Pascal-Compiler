{$mode objfpc}
program test_parent_find;

{ Test: overloaded method split across parent and child class hierarchy.
  find(ShortString) is on the parent, find(Word, Pointer) on the child.
  Both implicit self calls and dot-notation calls must resolve correctly. }

type
  TBase = class
  public
    function find(const s: ShortString): Integer; overload;
  end;

  TDerived = class(TBase)
  public
    function find(w: Word; p: Pointer): Integer; overload;
    procedure TestFind;
  end;

function TBase.find(const s: ShortString): Integer;
begin
  writeln('find(ShortString): ', s);
  Result := Length(s);
end;

function TDerived.find(w: Word; p: Pointer): Integer;
begin
  writeln('find(Word, Pointer): ', w);
  Result := w;
end;

procedure TDerived.TestFind;
var
  n: ShortString;
begin
  n := 'hello';
  writeln(find(n));
end;

var
  d: TDerived;
  n: ShortString;
begin
  d := TDerived.Create;
  { implicit self call from within the class }
  d.TestFind;
  { dot-notation call from outside the class }
  n := 'world';
  writeln(d.find(n));
  d.Free;
end.
