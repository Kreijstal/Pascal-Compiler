{ Test: Type helper with Int64 return calling overload }
{ BUG: Result becomes 'unsupported' when Int64 method calls overload }
program type_helper_int64_overload;

type
  TStringHelper = type helper for AnsiString
    function IndexOf(C: Char): Int64;
    function IndexOf(C: Char; Start: Int64): Int64;
  end;

function TStringHelper.IndexOf(C: Char; Start: Int64): Int64;
var
  I: Int64;
begin
  Result := -1;
  for I := Start to Length(Self) do
    if Self[I] = C then
    begin
      Result := I;
      Exit;
    end;
end;

function TStringHelper.IndexOf(C: Char): Int64;
begin
  Result := IndexOf(C, 1);  // Call overload - this triggers the bug
end;

var
  S: AnsiString;
  Pos: Int64;
begin
  S := 'Hello World';
  Pos := S.IndexOf('o');
  WriteLn(Pos);
end.
