program tdd_method_delete_insert_vs_builtin;

type
  TBox = class
  private
    FData: array[0..15] of Integer;
    FCount: Integer;
  public
    procedure Add(V: Integer);
    procedure Delete(Index: Integer);
    procedure Insert(Index, V: Integer);
    procedure RemoveValue(V: Integer);
    function Count: Integer;
    function Sum: Integer;
  end;

  THolder = class
    Box: TBox;
    procedure DropFirstAndInsertFront;
  end;

procedure TBox.Add(V: Integer);
begin
  FData[FCount] := V;
  Inc(FCount);
end;

procedure TBox.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    Exit;
  for I := Index to FCount - 2 do
    FData[I] := FData[I + 1];
  Dec(FCount);
end;

procedure TBox.Insert(Index, V: Integer);
var
  I: Integer;
begin
  if Index < 0 then
    Index := 0;
  if Index > FCount then
    Index := FCount;
  for I := FCount downto Index + 1 do
    FData[I] := FData[I - 1];
  FData[Index] := V;
  Inc(FCount);
end;

procedure TBox.RemoveValue(V: Integer);
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FData[I] = V then
    begin
      Self.Delete(I);
      Exit;
    end;
end;

function TBox.Count: Integer;
begin
  Result := FCount;
end;

function TBox.Sum: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FCount - 1 do
    Result := Result + FData[I];
end;

procedure THolder.DropFirstAndInsertFront;
begin
  Box.Delete(0);
  Box.Insert(0, 9);
end;

var
  B: TBox;
  H: THolder;
  S: string;
begin
  B := TBox.Create;
  H := THolder.Create;
  H.Box := B;
  B.Add(1);
  B.Add(2);
  B.Add(4);
  B.Insert(2, 3);
  B.RemoveValue(1);
  H.DropFirstAndInsertFront;
  H.Box.Delete(H.Box.Count - 1);
  Writeln(B.Count);
  Writeln(B.Sum);

  S := 'ABCDE';
  Delete(S, 2, 2);
  Insert('XYZ', S, 2);
  Writeln(S);

end.
