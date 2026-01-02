program type_helper_const_class;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TQWordHelper = type helper for QWord
  const
    MaxValue = High(QWord);
  public
    class function Size: Integer; static;
  end;

class function TQWordHelper.Size: Integer;
begin
  Result := 8;
end;

begin
  if QWord.MaxValue > 0 then
    writeln('ok');
  writeln(QWord.Size);
end.
