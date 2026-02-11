program heap_const_eval;

{$undef MinFixedHeaderAndPayload}
{$undef MaxFixedHeaderAndPayload}
{$undef FixedSizesCount}
{$undef SizeIndexBits}
{$undef SizeIndexMask}
{$undef VarSizeQuant}

const
  MinFixedHeaderAndPayload = 16;
  MaxFixedHeaderAndPayload = 544;
  FixedSizesCount = 16;
  FixedSizes: array[0..FixedSizesCount - 1] of word =
    (16, 32, 48, 64, 80, 96, 128, 160, 192, 224, 272, 320, 368, 416, 480, 544);
  SizeMinus1Div16ToIndex: array[0..(MaxFixedHeaderAndPayload - 1) div 16] of byte =
    (0, 1, 2, 3, 4, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 10, 11, 11, 11, 12,
     12, 12, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15);
  SizeIndexBits = 1 + trunc(ln(FixedSizesCount - 1) / ln(2));
  SizeIndexMask = (1 shl SizeIndexBits) - 1;
  VarSizeQuant = 1 shl (SizeIndexBits + 1);
  DivCheck = (MaxFixedHeaderAndPayload - 1) div 16;

var
  i, idx: integer;
  sumSizes: longint;
  sumIndexMap: longint;
  sumMapped: longint;
  sumMask: longint;

begin
  sumSizes := 0;
  for i := 0 to FixedSizesCount - 1 do
    sumSizes := sumSizes + FixedSizes[i];

  sumIndexMap := 0;
  for i := 0 to DivCheck do
    sumIndexMap := sumIndexMap + SizeMinus1Div16ToIndex[i];

  sumMapped := 0;
  for i := 1 to MaxFixedHeaderAndPayload do
  begin
    idx := SizeMinus1Div16ToIndex[(i - 1) div 16];
    sumMapped := sumMapped + FixedSizes[idx];
  end;

  sumMask := 0;
  for i := 0 to FixedSizesCount - 1 do
    sumMask := sumMask + (FixedSizes[i] and SizeIndexMask);

  writeln('bits=', SizeIndexBits);
  writeln('mask=', SizeIndexMask);
  writeln('quant=', VarSizeQuant);
  writeln('divcheck=', DivCheck);
  writeln('sumSizes=', sumSizes);
  writeln('sumIndexMap=', sumIndexMap);
  writeln('sumMapped=', sumMapped);
  writeln('sumMask=', sumMask);
end.
