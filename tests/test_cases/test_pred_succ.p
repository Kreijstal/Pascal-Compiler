program test_pred_succ;
var
    i: integer;
    li: longint;
begin
    { Test pred with integer }
    i := 10;
    i := pred(i);
    if i = 9 then
        writeln('pred(integer) works')
    else
        writeln('pred(integer) FAILED');
    
    { Test succ with integer }
    i := 5;
    i := succ(i);
    if i = 6 then
        writeln('succ(integer) works')
    else
        writeln('succ(integer) FAILED');
    
    { Test pred with longint }
    li := 100;
    li := pred(li);
    if li = 99 then
        writeln('pred(longint) works')
    else
        writeln('pred(longint) FAILED');
    
    { Test succ with longint }
    li := 50;
    li := succ(li);
    if li = 51 then
        writeln('succ(longint) works')
    else
        writeln('succ(longint) FAILED');
end.
