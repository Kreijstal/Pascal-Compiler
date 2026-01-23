{$mode objfpc}
{$H+}
{$modeswitch typehelpers}

program reg_widestring_helper_isempty;

type
  TWideHelper = type helper for WideString
  private
    function GetLength: SizeInt;
  public
    property Length: SizeInt read GetLength;
    function IsEmpty: Boolean;
  end;

function TWideHelper.GetLength: SizeInt;
begin
  Result := System.Length(Self);
end;

function TWideHelper.IsEmpty: Boolean;
begin
  Result := (Length = 0);
end;

var
  s: WideString;

begin
  s := '';
  WriteLn(s.IsEmpty);
end.
