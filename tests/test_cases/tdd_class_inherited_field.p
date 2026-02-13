program tdd_class_inherited_field;

{$mode objfpc}{$H+}

type
  TBase = class
  protected
    FSeed: Integer;
    FValues: array[0..3] of Integer;
  public
    constructor Create(ASeed: Integer);
    procedure Bump(Index, Delta: Integer);
    function Sum: Integer;
  end;

  TDerived = class(TBase)
  public
    function Mix(Iterations: Integer): Integer;
  end;

constructor TBase.Create(ASeed: Integer);
var
  I: Integer;
begin
  FSeed := ASeed;
  for I := 0 to High(FValues) do
    FValues[I] := ASeed + I;
end;

procedure TBase.Bump(Index, Delta: Integer);
begin
  FValues[Index] := FValues[Index] + Delta + FSeed;
end;

function TBase.Sum: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FValues) do
    Inc(Result, FValues[I]);
end;

function TDerived.Mix(Iterations: Integer): Integer;
var
  I: Integer;
  Idx: Integer;
begin
  Result := 0;
  for I := 1 to Iterations do
  begin
    Idx := (I + FSeed) mod (High(FValues) + 1);
    Bump(Idx, I);
    Inc(Result, Sum);
  end;
end;

var
  Obj: TDerived;
  Res: Integer;
begin
  Obj := TDerived.Create(7);
  try
    Res := Obj.Mix(5);
    Writeln('sum=', Obj.Sum);
    Writeln('mix=', Res);
  finally
    Obj.Free;
  end;
end.
