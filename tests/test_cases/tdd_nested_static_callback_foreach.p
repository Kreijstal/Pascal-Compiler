program tdd_nested_static_callback_foreach;

{$mode objfpc}

type
  TItem = class
  end;

  TObjectListCallback = procedure(Data: TItem; Arg: Pointer) of object;
  TObjectListStaticCallback = procedure(Data: TItem; Arg: Pointer);

  TObjectList = class
    procedure ForEachCall(Proc2Call: TObjectListCallback; Arg: Pointer);
    procedure ForEachCall(Proc2Call: TObjectListStaticCallback; Arg: Pointer);
  end;

  TSymTable = class
    SymList: TObjectList;
  end;

  TProcDef = class
    LocalSt: TSymTable;
  end;

  TProcInfo = class
    ProcDef: TProcDef;
  end;

procedure TObjectList.ForEachCall(Proc2Call: TObjectListCallback; Arg: Pointer);
begin
  Writeln('wrong overload');
end;

procedure TObjectList.ForEachCall(Proc2Call: TObjectListStaticCallback; Arg: Pointer);
begin
  Proc2Call(TItem(Pointer(11)), Arg);
end;

var
  CurrentProcInfo: TProcInfo;

procedure Run;
  procedure Visit(Data: TItem; Arg: Pointer);
  begin
    Writeln('visit ', PtrUInt(Data), ' ', PtrUInt(Arg));
  end;

begin
  CurrentProcInfo := TProcInfo.Create;
  CurrentProcInfo.ProcDef := TProcDef.Create;
  CurrentProcInfo.ProcDef.LocalSt := TSymTable.Create;
  CurrentProcInfo.ProcDef.LocalSt.SymList := TObjectList.Create;
  CurrentProcInfo.ProcDef.LocalSt.SymList.ForEachCall(@Visit, Pointer(7));
  Writeln('done');
end;

begin
  Run;
end.
