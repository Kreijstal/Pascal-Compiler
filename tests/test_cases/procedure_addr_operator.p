program procedure_addr_operator;

type
  TCallback = procedure;

var
  cb: TCallback;
  alt: TCallback;

procedure EmitGreeting;
begin
  writeln('callback invoked');
end;

begin
  cb := @EmitGreeting;
  writeln('Direct callback:');
  cb;

  alt := @EmitGreeting;
  writeln('Second callback:');
  alt;
end.
