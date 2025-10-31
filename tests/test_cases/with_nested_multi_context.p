program WithNestedMultiContext;

type
  TFirst = record
    Alpha: Integer;
  end;

  TSecond = record
    Beta: Integer;
  end;

var
  First: TFirst;
  Second: TSecond;
  ResultValue: Integer;
begin
  First.Alpha := 5;
  Second.Beta := 7;
  ResultValue := 0;

  with First do
  begin
    Alpha := Alpha + 1;
    ResultValue := Alpha;
  end;

  with Second do
  begin
    Beta := Beta + ResultValue;
    ResultValue := ResultValue + Beta;
  end;

  with First do
  begin
    with Second do
    begin
      ResultValue := ResultValue + Alpha + Beta;
    end;
  end;

  with First, Second do
  begin
    ResultValue := ResultValue + Alpha + Beta;
  end;

  writeln(First.Alpha);
  writeln(Second.Beta);
  writeln(ResultValue);
end.
