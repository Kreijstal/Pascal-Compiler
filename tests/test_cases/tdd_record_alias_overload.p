{ TDD: record alias should resolve correct overload }
{$mode objfpc}
program tdd_record_alias_overload;

type
  TRaw = record
    A: Integer;
  end;
  TUni = record
    A: Integer;
  end;
  TSearch = TRaw;

procedure FindClose(var R: TRaw);
begin
  WriteLn('raw');
end;

procedure FindClose(var R: TUni);
begin
  WriteLn('uni');
end;

var
  S: TSearch;

begin
  FindClose(S);
end.
