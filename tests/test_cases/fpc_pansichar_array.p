program FPCPCharArray;
{
  Test PChar array (array of string pointers) and StrPas conversion.
  This pattern is used in FPC RTL's errors.pp:
    sys_errlist:array[0..sys_errn-1] of PAnsiChar = (...);
    StrError:=StrPas(Sys_ErrList[err]);
}

const
  NUM_MSGS = 3;
  msg_list: array[0..2] of PChar = (
    'Success',
    'Error occurred',
    'Fatal error'
  );

function GetMessage(index: integer): string;
begin
  if (index >= 0) and (index < NUM_MSGS) then
    GetMessage := StrPas(msg_list[index])
  else
    GetMessage := 'Unknown';
end;

var
  i: integer;

begin
  for i := 0 to NUM_MSGS - 1 do
    WriteLn('Message ', i, ': ', GetMessage(i));
  
  WriteLn('Invalid index: ', GetMessage(99));
  WriteLn('Test completed');
end.
