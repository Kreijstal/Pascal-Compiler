program object_const_eval;

{$mode objfpc}
{$undef MinFixedHeaderAndPayload}
{$undef MaxFixedHeaderAndPayload}
{$undef FixedSizesCount}
{$undef SizeIndexBits}
{$undef SizeIndexMask}
{$undef VarSizeQuant}

type
  THeapLike = object
    const
      FixedSizesCount = 4;
      FixedSizes: array[0..FixedSizesCount - 1] of word = (16, 32, 48, 64);
      SizeIndexBits = 1 + trunc(ln(FixedSizesCount - 1) / ln(2));
      SizeIndexMask = (1 shl SizeIndexBits) - 1;
      SizeMinus1Div16ToIndex: array[0..(64 - 1) div 16] of byte = (0, 1, 2, 3);
  end;

var
  i, idx: integer;
  sumSizes: longint;
  sumMap: longint;
  sumMasked: longint;

begin
  sumSizes := 0;
  for i := 0 to THeapLike.FixedSizesCount - 1 do
    sumSizes := sumSizes + THeapLike.FixedSizes[i];

  sumMap := 0;
  for i := 0 to (64 - 1) div 16 do
    sumMap := sumMap + THeapLike.SizeMinus1Div16ToIndex[i];

  sumMasked := 0;
  for i := 1 to 64 do
  begin
    idx := THeapLike.SizeMinus1Div16ToIndex[(i - 1) div 16];
    sumMasked := sumMasked + (THeapLike.FixedSizes[idx] and THeapLike.SizeIndexMask);
  end;

  writeln('bits=', THeapLike.SizeIndexBits);
  writeln('mask=', THeapLike.SizeIndexMask);
  writeln('sumSizes=', sumSizes);
  writeln('sumMap=', sumMap);
  writeln('sumMasked=', sumMasked);
end.
