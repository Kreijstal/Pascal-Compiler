program Int64MultiplicationTest;
{ Test Int64 multiplication to verify 64-bit operations are used correctly }
var
    a: Int64;
    b: Int64;
    c: Int64;
begin
    { Test large value multiplication that would overflow 32-bit }
    a := 1734292246;
    b := 1000;
    c := a * b;
    writeln(c);
    
    { Test smaller values }
    a := 100;
    b := 200;
    c := a * b;
    writeln(c);
end.
