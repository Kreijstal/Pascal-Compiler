program TestWithMethodResult;
{$mode objfpc}

type
  TMyList = class(TObject)
  private
    FItems: array of Pointer;
    FCount: Integer;
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; Value: Pointer);
  public
    constructor Create;
    function Add(Item: Pointer): Integer;
    property Count: Integer read FCount;
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
  end;

  TThreadList = class(TObject)
  private
    FList: TMyList;
  public
    constructor Create;
    destructor Destroy; override;
    function LockList: TMyList;
  end;

  TIntConst = class(TObject)
  public
    IntegerType: Pointer;
    IntValue: Integer;
    constructor Create(AType: Pointer; AValue: Integer);
  end;

constructor TMyList.Create;
begin
  inherited;
  FCount := 0;
end;

function TMyList.GetItem(Index: Integer): Pointer;
begin
  Result := FItems[Index];
end;

procedure TMyList.SetItem(Index: Integer; Value: Pointer);
begin
  FItems[Index] := Value;
end;

function TMyList.Add(Item: Pointer): Integer;
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

constructor TThreadList.Create;
begin
  inherited;
  FList := TMyList.Create;
end;

destructor TThreadList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TThreadList.LockList: TMyList;
begin
  Result := FList;
end;

constructor TIntConst.Create(AType: Pointer; AValue: Integer);
begin
  inherited Create;
  IntegerType := AType;
  IntValue := AValue;
end;

var
  IntConstList: TThreadList;
  ic: TIntConst;
  found: Boolean;
  i: Integer;
  p: Pointer;
begin
  IntConstList := TThreadList.Create;

  ic := TIntConst.Create(Pointer(1), 100);
  IntConstList.LockList.Add(ic);
  ic := TIntConst.Create(Pointer(2), 200);
  IntConstList.LockList.Add(ic);
  ic := TIntConst.Create(Pointer(1), 300);
  IntConstList.LockList.Add(ic);

  { Pattern from classes.pp: with LockList do try...for...end }
  found := False;
  p := Pointer(1);
  with IntConstList.LockList do
  begin
    for i := 0 to Count - 1 do
    begin
      if TIntConst(Items[i]).IntegerType = p then
      begin
        WriteLn('Found IntValue=', TIntConst(Items[i]).IntValue, ' at index ', i);
        found := True;
      end;
    end;
  end;

  if found then
    WriteLn('Test PASSED: found matching items')
  else
    WriteLn('Test FAILED: no matching items found');

  { Clean up }
  with IntConstList.LockList do
    for i := 0 to Count - 1 do
      TObject(Items[i]).Free;
  IntConstList.Free;
end.
