program tdd_finalize_count_arg;

type
  TStringArray = array[0..1] of ansistring;

var
  values: TStringArray;

begin
  values[0] := 'a';
  values[1] := 'b';
  Finalize(values, 2);
  Writeln('ok');
end.
