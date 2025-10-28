unit Math;

interface

function Max(a, b: integer): integer;
function Max(a, b: longint): longint;

implementation

function Max(a, b: integer): integer;
begin
    if a >= b then
        Max := a
    else
        Max := b;
end;

function Max(a, b: longint): longint;
begin
    if a >= b then
        Max := a
    else
        Max := b;
end;

end.
