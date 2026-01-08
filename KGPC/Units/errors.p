unit errors;
interface

function StrError(errno: Integer): String;

implementation

function IntToStr(i: Integer): String;
var
  s: String;
begin
  Str(i, s);
  Result := s;
end;

function StrError(errno: Integer): String;
begin
  Result := 'Error ' + IntToStr(errno);
end;

end.
