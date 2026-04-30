program TddShortStringEquality;
{$H-}

var
  A, B: string;
  P: ^string;
  U: string;

begin
  A := 'SYSTEM';
  B := 'SYSTEM';
  Writeln(Ord(A = B));
  Writeln(Ord(A <> B));
  B := 'SYZZZZ';
  Writeln(Ord(A = B));
  Writeln(Ord(A <> B));
  New(P);
  P^ := 'SYSTEM';
  U := 'SYZZZZ';
  Writeln(Ord(P^ = U));
  Writeln(Ord(P^ <> U));
end.
