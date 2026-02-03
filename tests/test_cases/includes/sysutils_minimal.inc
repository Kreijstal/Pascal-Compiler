{ Minimal include file mimicking sysutils.inc }
{ These procedures have NO forward declaration in the interface }

Procedure InitExceptions;
begin
  WriteLn('InitExceptions');
end;

Procedure DoneExceptions;
begin
  WriteLn('DoneExceptions');
end;
