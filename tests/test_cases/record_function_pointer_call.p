program record_function_pointer_call;

type
  TMyFunc = function(const s: string): string;
  
  TMyRecord = record
    MyFunc: TMyFunc;
  end;

var
  Manager: TMyRecord;

function DoSomething(const s: string): string;
begin
  DoSomething := s;
end;

var
  c: char;
begin
  Manager.MyFunc := @DoSomething;
  c := Manager.MyFunc('hello')[1];
  writeln('FirstChar=', c);
end.
