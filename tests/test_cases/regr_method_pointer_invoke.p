program regr_method_pointer_invoke;

{$mode objfpc}

uses sysutils;

type
  TGreeter = class
    Name: AnsiString;
    procedure Hi;
  end;

procedure TGreeter.Hi;
begin
  writeln('Hello, ', Name);
end;

var
  g: TGreeter;
  m: procedure of object;
begin
  g := TGreeter.Create;
  g.Name := 'World';
  m := @g.Hi;
  m;
  g.Free;
end.
