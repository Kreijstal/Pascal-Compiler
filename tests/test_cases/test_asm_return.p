program test_asm_function;

function GetPi: real;
begin
    asm
        fldpi
        fstpl -16(%rbp)
    end;
end;

function GetAnswer: integer;
begin
    asm
        movl $42, -12(%rbp)
    end;
end;

begin
    writeln('Pi = ', GetPi:0:5);
    writeln('The answer is ', GetAnswer);
end.
