unit FGL;

{$mode objfpc}

interface

uses SysUtils;

type
  generic TFPGListEnumerator<T> = record
  private
    FItems: array of T;
    FCount: NativeInt;
    FIndex: NativeInt;
  public
    function MoveNext: Boolean;
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  generic TFPGList<T> = class
  private
    FItems: array of T;
    FCount: NativeInt;
    function GetItem(Index: NativeInt): T;
    procedure SetItem(Index: NativeInt; const Value: T);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Value: T);
    function Count: NativeInt;
    function GetEnumerator: TFPGListEnumerator<T>;
    property Items[Index: NativeInt]: T read GetItem write SetItem; default;
  end;

implementation

{ TFPGList<T> }

constructor TFPGList<T>.Create;
begin
  inherited Create;
  FCount := 0;
  SetLength(FItems, 0);
end;

destructor TFPGList<T>.Destroy;
begin
  SetLength(FItems, 0);
  inherited Destroy;
end;

procedure TFPGList<T>.Add(const Value: T);
var
  NewCapacity: NativeInt;
begin
  if Length(FItems) <= FCount then
  begin
    NewCapacity := Length(FItems) * 2;
    if NewCapacity = 0 then
      NewCapacity := 4;
    SetLength(FItems, NewCapacity);
  end;
  FItems[FCount] := Value;
  Inc(FCount);
end;

function TFPGList<T>.Count: NativeInt;
begin
  Result := FCount;
end;

function TFPGList<T>.GetItem(Index: NativeInt): T;
begin
  Result := FItems[Index];
end;

procedure TFPGList<T>.SetItem(Index: NativeInt; const Value: T);
begin
  FItems[Index] := Value;
end;

function TFPGList<T>.GetEnumerator: TFPGListEnumerator<T>;
begin
  Result.FItems := Self.FItems;
  Result.FCount := Self.FCount;
  Result.FIndex := 0;
end;

{ TFPGListEnumerator<T> }

function TFPGListEnumerator<T>.MoveNext: Boolean;
begin
  Result := FIndex < FCount;
  if Result then
    Inc(FIndex);
end;

function TFPGListEnumerator<T>.GetCurrent: T;
begin
  Result := FItems[FIndex - 1];
end;

end.
