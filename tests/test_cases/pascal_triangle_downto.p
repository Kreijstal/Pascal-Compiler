program TestPascalTriangle;

{ Direct test of Pascal's triangle using downto }

var
  pasTri: array[0..5] of Integer;
  i, k: Integer;

begin
  { Initialize }
  pasTri[0] := 1;
  pasTri[1] := 0;
  pasTri[2] := 0;
  pasTri[3] := 0;
  pasTri[4] := 0;
  pasTri[5] := 0;
  
  { Row 0 }
  WriteLn('Row 0: ', pasTri[0]);
  
  { Build row 1 }
  for k := 1 downto 1 do
    pasTri[k] := pasTri[k] + pasTri[k - 1];
  WriteLn('Row 1: ', pasTri[0], ' ', pasTri[1]);
  
  { Build row 2 }
  for k := 2 downto 1 do
    pasTri[k] := pasTri[k] + pasTri[k - 1];
  WriteLn('Row 2: ', pasTri[0], ' ', pasTri[1], ' ', pasTri[2]);
  
  { Build row 3 }
  for k := 3 downto 1 do
    pasTri[k] := pasTri[k] + pasTri[k - 1];
  WriteLn('Row 3: ', pasTri[0], ' ', pasTri[1], ' ', pasTri[2], ' ', pasTri[3]);
  
  { Build row 4 }
  for k := 4 downto 1 do
    pasTri[k] := pasTri[k] + pasTri[k - 1];
  WriteLn('Row 4: ', pasTri[0], ' ', pasTri[1], ' ', pasTri[2], ' ', pasTri[3], ' ', pasTri[4]);
end.
