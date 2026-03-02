program test_with_method_call_context;
{$mode objfpc}
{ Test: WITH context method resolution for methods called on return values.
  Exercises the pattern:
    with SomeObj.GetInnerObj do
      InnerMethod(arg);
  where InnerMethod is a method of the return type of GetInnerObj.
  This tests:
  1. Method calls resolved through WITH context (Add, Remove, IndexOf)
  2. Property access through WITH context (Count)
  3. WITH on method call return value (LockList pattern)
  4. try/finally inside WITH block
  5. Nested class method resolution via WITH }

type
  TItem = class
  private
    FName: String;
  public
    constructor Create(const AName: String);
    property Name: String read FName;
  end;

  TItemList = class
  private
    FItems: array[0..15] of TItem;
    FCount: Integer;
  public
    function Add(Item: TItem): Integer;
    function Remove(Item: TItem): Integer;
    function IndexOf(Item: TItem): Integer;
    property Count: Integer read FCount;
  end;

  TThreadSafeList = class
  private
    FList: TItemList;
    FLocked: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function LockList: TItemList;
    procedure UnlockList;
  end;

  TWorker = class
  private
    FRegistry: TThreadSafeList;
    FItem: TItem;
  public
    constructor Create(const AName: String);
    destructor Destroy; override;
    procedure Register;
    procedure Unregister;
  end;

constructor TItem.Create(const AName: String);
begin
  inherited Create;
  FName := AName;
end;

function TItemList.Add(Item: TItem): Integer;
begin
  FItems[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

function TItemList.Remove(Item: TItem): Integer;
var
  i: Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
  begin
    Dec(FCount);
    for i := Result to FCount - 1 do
      FItems[i] := FItems[i + 1];
    FItems[FCount] := nil;
  end;
end;

function TItemList.IndexOf(Item: TItem): Integer;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if FItems[i] = Item then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

constructor TThreadSafeList.Create;
begin
  inherited Create;
  FList := TItemList.Create;
  FLocked := False;
end;

destructor TThreadSafeList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TThreadSafeList.LockList: TItemList;
begin
  FLocked := True;
  Result := FList;
end;

procedure TThreadSafeList.UnlockList;
begin
  FLocked := False;
end;

constructor TWorker.Create(const AName: String);
begin
  inherited Create;
  FRegistry := TThreadSafeList.Create;
  FItem := TItem.Create(AName);
end;

destructor TWorker.Destroy;
begin
  FItem.Free;
  FRegistry.Free;
  inherited Destroy;
end;

procedure TWorker.Register;
begin
  { This is the key pattern from classes.pp:
    with FRegistry.LockList do
      try
        Add(FItem);
      finally
        FRegistry.UnlockList;
      end;
    Add must resolve as TItemList.Add via WITH context. }
  with FRegistry.LockList do
    try
      Add(FItem);
    finally
      FRegistry.UnlockList;
    end;
end;

procedure TWorker.Unregister;
begin
  with FRegistry.LockList do
    try
      Remove(FItem);
    finally
      FRegistry.UnlockList;
    end;
end;

var
  w1, w2: TWorker;
begin
  w1 := TWorker.Create('Worker1');
  w2 := TWorker.Create('Worker2');
  w1.Register;
  w2.Register;
  { Verify items are registered }
  WriteLn(w1.FRegistry.LockList.Count);
  w1.FRegistry.UnlockList;
  { Unregister one }
  w1.Unregister;
  WriteLn(w1.FRegistry.LockList.Count);
  w1.FRegistry.UnlockList;
  w2.Unregister;
  WriteLn(w1.FRegistry.LockList.Count);
  w1.FRegistry.UnlockList;
  w2.Free;
  w1.Free;
end.
