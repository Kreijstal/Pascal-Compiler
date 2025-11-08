program array_function_return;

type
    TStringArray = array of string;

function BuildNames(const Prefix: string): TStringArray;
var
    result: TStringArray;
begin
    SetLength(result, 2);
    result[0] := Prefix + 'A';
    result[1] := Prefix + 'B';
    BuildNames := result;
end;

var
    Values: TStringArray;
begin
    Values := BuildNames('X');
end.
