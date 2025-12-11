program FPCInlineFunction;
{
  Test inline function directive.
  This is used throughout FPC RTL:
    function getunicode(c : AnsiChar;p : punicodemap) : tunicodechar;inline;
}

function AddOne(x: integer): integer; inline;
begin
  AddOne := x + 1;
end;

function DoubleValue(x: integer): integer; inline;
begin
  DoubleValue := x * 2;
end;

var
  val: integer;

begin
  val := 5;
  WriteLn('Original: ', val);
  
  val := AddOne(val);
  WriteLn('After AddOne: ', val);
  
  val := DoubleValue(val);
  WriteLn('After Double: ', val);
  
  WriteLn('Test completed');
end.
