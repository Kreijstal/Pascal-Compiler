{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
program reg_string_helper_self_assign;

type
  TStringHelper = type helper for AnsiString
    procedure SetFirst(C: Char);
  end;

procedure TStringHelper.SetFirst(C: Char);
begin
  Self[1] := C;
end;

var
  S: AnsiString;
begin
  S := 'abc';
  S.SetFirst('z');
  WriteLn(S);
end.
