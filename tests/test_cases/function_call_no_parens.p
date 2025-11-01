program FunctionCallNoParens;

uses SysUtils;

procedure Foo;
begin
end;

function Bar: integer;
begin
  Bar := 1;
end;

function GetFortyTwo: integer;
begin
  GetFortyTwo := 42;
end;

begin
  Foo;
  writeln(Bar);
  writeln(GetFortyTwo);
  writeln('Sum: ', Bar + GetFortyTwo);
end.
