program fpc_bootstrap_relop_spill_movelist;
{$mode objfpc}

type
  TData = array[0..31] of Pointer;
  PMoveList = ^TMoveList;

  THeader = record
    maxcount: LongInt;
    count: LongInt;
    sorted_until: LongInt;
  end;

  TMoveList = record
    header: THeader;
    data: TData;
  end;

  TRegInfo = record
    movelist: PMoveList;
  end;

  TRegInfoArray = array[0..3] of TRegInfo;

  TRegObj = class
    reginfo: TRegInfoArray;
    procedure AddToMoveList(u: LongInt; data: Pointer);
  end;

procedure TRegObj.AddToMoveList(u: LongInt; data: Pointer);
begin
  with reginfo[u] do
    begin
      if movelist = nil then
        begin
          GetMem(movelist, SizeOf(TMoveList));
          movelist^.header.maxcount := 16;
          movelist^.header.count := 0;
          movelist^.header.sorted_until := 0;
        end
      else
        begin
          if movelist^.header.count >= movelist^.header.maxcount then
            begin
              movelist^.header.maxcount := movelist^.header.maxcount * 2;
              ReAllocMem(movelist, PtrUInt(@movelist^.data) - PtrUInt(movelist)
                + movelist^.header.maxcount * SizeOf(Pointer));
            end;
        end;
      movelist^.data[movelist^.header.count] := data;
      Inc(movelist^.header.count);
    end;
end;

var
  obj: TRegObj;
  i: LongInt;
  ok: Boolean;

begin
  obj := TRegObj.Create;
  for i := 1 to 20 do
    obj.AddToMoveList(0, Pointer(PtrUInt(i)));

  ok := obj.reginfo[0].movelist^.header.count = 20;
  ok := ok and (obj.reginfo[0].movelist^.header.maxcount = 32);
  ok := ok and (obj.reginfo[0].movelist^.data[19] = Pointer(PtrUInt(20)));

  if ok then
    writeln('ok')
  else
    writeln('bad');
end.
