procedure TriggerIncludeError;
type
  TRec = record
    Value: Integer;
  end;

var
  R: TRec;
  Items: array[0..2] of Integer;
  I: Integer;

  function MakeValue(Index: Integer): Integer;
  begin
    Result := Index * 3 + 1;
  end;

begin
  for I := 0 to High(Items) do
    Items[I] := MakeValue(I);
  R.Value := 'bad';
end;
