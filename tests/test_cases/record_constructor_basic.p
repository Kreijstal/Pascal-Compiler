program RecordConstructorBasic;

type
  TPoint = record
    X: Integer;
    Y: Integer;
  end;

  TLine = record
    A: TPoint;
    B: TPoint;
  end;

procedure UsePoint(const P: TPoint);
begin
  WriteLn('UsePoint:', P.X, ',', P.Y);
end;

var
  P: TPoint = (X: 1; Y: 2);
  Q: TPoint;
  L: TLine;
  Arr: array[0..1] of TPoint;
begin
  Q := (Y: 4; X: 3);
  WriteLn('P:', P.X, ',', P.Y);
  WriteLn('Q:', Q.X, ',', Q.Y);

  UsePoint((X: 5; Y: 6));

  L := (A: (X: 7; Y: 8); B: (X: 9; Y: 10));
  WriteLn('L:', L.A.X, ',', L.A.Y, ';', L.B.X, ',', L.B.Y);

  Arr[0] := (X: 11; Y: 12);
  Arr[1] := (X: 13; Y: 14);
  WriteLn('Arr0:', Arr[0].X, ',', Arr[0].Y);
  WriteLn('Arr1:', Arr[1].X, ',', Arr[1].Y);
end.
