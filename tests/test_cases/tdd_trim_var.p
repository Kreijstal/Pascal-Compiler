program tdd_trim_var;

type
  TOther = class
    class function Trim(const S: string): string; static;
  end;

  TThread = class
    class function GetLine: string; static;
  end;

function Trim(const S: string): string;
begin
  Result := 'global:' + S;
end;

class function TOther.Trim(const S: string): string;
begin
  Result := 'other:' + S;
end;

class function TThread.GetLine: string;
var
  L: string;
  function GetNextWord(var S: string): string;
  begin
    S := Trim(S);
    Result := S;
  end;
begin
  L := 'hi';
  Result := GetNextWord(L);
end;

begin
  Writeln(TThread.GetLine);
end.
