program tdd_class_shortstring_concat;
{$mode objfpc}
{$H-}

type
  TOption = class
  public
    HelpLine: string;
    procedure Check;
  end;

procedure TOption.Check;
var
  i: integer;
begin
  WriteLn(HelpLine);
  for i := 1 to Length(HelpLine) do
    if Ord(HelpLine[i]) < 32 then
    begin
      WriteLn('CORRUPTION at ', i);
      Halt(1);
    end;
end;

var
  opt: TOption;
  s1, s2: string;
begin
  opt := TOption.Create;
  s1 := 'Hello';
  s2 := ' World';
  opt.HelpLine := s1 + s2;
  opt.Check;
  opt.Free;
end.
