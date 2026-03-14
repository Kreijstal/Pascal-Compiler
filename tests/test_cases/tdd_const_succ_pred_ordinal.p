program tdd_const_succ_pred_ordinal;

type
  TToken = (NOTOKEN, FIRSTTOKEN, SECONDTOKEN);
  TSmall = -1..1;

const
  FirstOverloaded = Succ(NOTOKEN);
  BackToFirst = Pred(SECONDTOKEN);
  CastedToken = TToken(1);
  NextSubrange = Succ(TSmall(0));

begin
  writeln(Ord(FirstOverloaded));
  writeln(Ord(BackToFirst));
  writeln(Ord(CastedToken));
  writeln(NextSubrange);
end.
