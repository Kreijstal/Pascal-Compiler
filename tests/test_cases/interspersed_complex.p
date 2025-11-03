program InterspersedComplex;

const
  MyConst = 10;

procedure DoSomething;
begin
  writeln('Something');
end;

type
  TMyRecord = record
    a: integer;
    b: string;
  end;

var
  x: integer;

procedure DoSomethingElse;
begin
  writeln('Something else');
end;

var
  y: TMyRecord;

begin
  x := MyConst;
  y.a := x;
  y.b := 'hello';
  DoSomething;
  DoSomethingElse;
end.
