program FPCStrProcedure;
{
  Test the str() built-in procedure for converting numbers to strings.
  This is used in FPC RTL's errors.pp:
    str(err,s);
}

var
  num: integer;
  s: string;

begin
  num := 42;
  str(num, s);
  WriteLn('Converted: ', s);
  
  num := -999;
  str(num, s);
  WriteLn('Negative: ', s);
  
  WriteLn('Test completed');
end.
