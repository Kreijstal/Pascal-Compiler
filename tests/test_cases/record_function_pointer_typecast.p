program record_function_pointer_typecast;

type
  TMyFunc = function(const s: string): string;
  
  TMyRecord = record
    MyFunc: TMyFunc;
  end;

function EchoString(const s: string): string;
begin
  EchoString := s;
end;

var
  Manager: TMyRecord;
  s: string;
begin
  Manager.MyFunc := @EchoString;
  s := Manager.MyFunc('hello');
  writeln(s);
end.
