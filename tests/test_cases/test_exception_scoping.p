program test_exception_scoping;

type
    MyException = class
        constructor Create(dummy: Integer);
    end;

constructor MyException.Create(dummy: Integer);
begin
end;

var
    E: Integer;  { Global variable E }
    ex: MyException;

begin
    E := 42;     { Set global E }
    
    try
        WriteLn('In try block, E = ', E);  { Should print 42 }
        ex := MyException.Create(0);
        raise ex;
    except
        on E: MyException do
        begin
            WriteLn('In except block, E is the exception variable');
            { E here should be the exception variable, not the global }
        end;
    end;
    
    WriteLn('After except, E = ', E);  { Should print 42 again }
end.
