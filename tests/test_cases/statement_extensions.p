program StatementExtensions;

type
  TFruit = record
    placeholder: integer;
  end;

var
  fruit: TFruit;
  counter: integer;
  total: integer;
  inheritedCalls: integer;

procedure BaseCreate(var totalRef: integer; var inheritedRef: integer);
begin
  totalRef := totalRef + 10;
  inheritedRef := inheritedRef + 1;
end;

procedure DerivedCreate(var totalRef: integer; var inheritedRef: integer);
begin
  inherited BaseCreate(totalRef, inheritedRef);
end;

begin
  counter := 0;
  total := 0;
  inheritedCalls := 0;

  with fruit do
  begin
    total := total + 1;
  end;

  while counter < 5 do
  begin
    counter := counter + 1;
    if counter = 3 then
      break;
  end;

  try
    DerivedCreate(total, inheritedCalls);
    total := total + 10;
    total := total + 1;
    inheritedCalls := inheritedCalls + 1;
  finally
    total := total + 100;
  end;

  try
    if counter < 0 then
      raise 123;
  except
    total := -1;
  end;

  writeln(total);
  writeln(inheritedCalls);
  writeln(counter);
end.
