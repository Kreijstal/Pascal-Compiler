program tdd_getlastcasterrorinfo;

{$mode objfpc}

{$if defined(FPC) and not defined(KGPC)}
type
  TObjectHelper = class helper for TObject
    class procedure GetLastCastErrorInfo(out aFrom, aTo: shortstring); static;
  end;

class procedure TObjectHelper.GetLastCastErrorInfo(out aFrom, aTo: shortstring);
begin
  aFrom := '';
  aTo := '';
end;
{$endif}

type
  TA = class(TObject);
  TB = class(TA);

var
  A: TA;
  B: TB;
  F, T: ShortString;
  I: Integer;
  Sum: Integer;

procedure ConsumeCastInfo;
begin
  TObject.GetLastCastErrorInfo(F, T);
  for I := 1 to Length(F) do
    Sum := Sum + Ord(F[I]);
  for I := 1 to Length(T) do
    Sum := Sum + Ord(T[I]);
end;

begin
  TObject.GetLastCastErrorInfo(F, T);
  if (F <> '') or (T <> '') then
    begin
      Writeln('Error, cast info must be empty at start');
      Halt(1);
    end;
  Sum := 0;
  A := TB.Create;
  try
    B := A as TB;
  except
    ConsumeCastInfo;
    Writeln('Unexpected cast failure');
    Halt(2);
  end;
  ConsumeCastInfo;
  Writeln('Cast info empty, checksum=', Sum);
end.
