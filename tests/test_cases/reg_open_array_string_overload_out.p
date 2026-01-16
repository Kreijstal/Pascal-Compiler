program reg_open_array_string_overload_out;

{$mode objfpc}
{$H+}

function IndexOfAny(const AnyOf: array of Char; StartIndex: Integer; Count: Integer; out Match: Integer): Integer; overload;
begin
  Match := 111;
  Result := 1;
end;

function IndexOfAny(const AnyOf: array of String; StartIndex: Integer; Count: Integer; out Match: Integer): Integer; overload;
begin
  Match := 222;
  Result := 2;
end;

var
  m: Integer;
  r: Integer;
begin
  r := IndexOfAny(['aa', 'bb'], 0, 2, m);
  writeln('r=', r, ' m=', m);
  r := IndexOfAny(['a', 'b'], 0, 2, m);
  writeln('r=', r, ' m=', m);
end.
