program TddShortStringEquality;
{$H-}

var
  A, B: string;

begin
  A := 'SYSTEM';
  B := 'SYSTEM';
  Writeln(Ord(A = B));
  Writeln(Ord(A <> B));
end.
