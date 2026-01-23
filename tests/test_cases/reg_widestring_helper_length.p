{$mode objfpc}
{$H+}
{$modeswitch typehelpers}

program reg_widestring_helper_length;

type
  TWideHelper = type helper for WideString
  private
    function GetLength: SizeInt;
  public
    property Length: SizeInt read GetLength;
    function GetLen: SizeInt;
  end;

function TWideHelper.GetLength: SizeInt;
begin
  Result := System.Length(Self);
end;

function TWideHelper.GetLen: SizeInt;
begin
  Result := Length;
end;

var
  s: WideString;

begin
  s := 'hi';
  WriteLn(s.GetLen);
end.
