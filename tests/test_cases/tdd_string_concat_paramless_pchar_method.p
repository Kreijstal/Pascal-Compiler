{$mode objfpc}
program tdd_string_concat_paramless_pchar_method;

type
  THolder = object
    function AsConstPChar: PChar;
  end;

function THolder.AsConstPChar: PChar;
begin
  Result := 'abc';
end;

var
  H: THolder;
  S: string;
begin
  S := '''' + H.AsConstPChar + '''';
  WriteLn(S);
end.
