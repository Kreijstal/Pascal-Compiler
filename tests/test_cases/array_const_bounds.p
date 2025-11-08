program array_const_bounds;
const 
    N = 10;
    START = 1;
var 
    arr1: array[1..N] of integer;
    arr2: array[START..N] of integer;
    i: integer;
begin
    { Test array with constant upper bound }
    arr1[1] := 42;
    arr1[5] := 100;
    arr1[N] := 999;
    
    { Test array with constant start and end bounds }
    arr2[START] := 11;
    arr2[5] := 55;
    arr2[N] := 110;
    
    { Verify values }
    if arr1[1] <> 42 then halt(1);
    if arr1[5] <> 100 then halt(2);
    if arr1[10] <> 999 then halt(3);
    
    if arr2[1] <> 11 then halt(4);
    if arr2[5] <> 55 then halt(5);
    if arr2[10] <> 110 then halt(6);
    
    writeln('All array constant bounds tests passed!');
end.
