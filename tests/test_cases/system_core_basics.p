program system_core_basics;

type
  CharSet = set of char;

var
  absInt: Integer;
  s: CharSet;
  i: Integer;
begin
  absInt := Abs(-42);
  writeln('AbsInt=', absInt);
  writeln('AbsReal=', Abs(-3.5):0:2);
  writeln('Sqrt=', Sqrt(2.25):0:2);
  writeln('Sin=', Sin(0.0):0:2);
  writeln('Cos=', Cos(0.0):0:2);
  writeln('ArcTan=', ArcTan(1.0):0:3);
  writeln('RoundPos=', Round(2.6));
  writeln('RoundNeg=', Round(-2.4));
  writeln('TruncNeg=', Trunc(-2.4));
  writeln('IntNeg=', Int(-2.4));
  writeln('FracNeg=', Frac(-2.25):0:2);

  i := 5;
  Dec(i);
  Dec(i, 2);
  writeln('DecValue=', i);
  Inc(i, 4);
  writeln('IncValue=', i);

  s := [];
  Include(s, 'a');
  Include(s, 'c');
  Exclude(s, 'a');
  if 'a' in s then writeln('HasA=1') else writeln('HasA=0');
  if 'c' in s then writeln('HasC=1') else writeln('HasC=0');
  Exclude(s, 'c');
  if 'c' in s then writeln('PostExcludeC=1') else writeln('PostExcludeC=0');
end.
