program bug_addr_of_self_method_overload;
{$mode objfpc}

type
  TBase = class
    procedure ExchangeItems(Index1, Index2: Integer); virtual;
    procedure Check;
  end;

procedure TBase.ExchangeItems(Index1, Index2: Integer);
begin
end;

procedure TBase.Check;
begin
  if TMethod(@Self.ExchangeItems).Code = CodePointer(@TBase.ExchangeItems) then
  begin
  end
  else
  begin
  end;
end;

var
  O: TBase;
begin
  O := TBase.Create;
  O.Check;
  O.Free;
end.
