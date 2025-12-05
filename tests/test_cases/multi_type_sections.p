program multi_type_sections;
{ Regression test: multiple type sections interspersed with const/var must all be parsed.
  This is required for FPC system.pp compatibility where ~20+ type sections exist. }

type
  TA = Integer;
  TB = Integer;

const
  C1 = 100;
  
type
  TC = Integer;
  TD = Integer;

var
  globalVar: Integer;

type
  TE = Integer;
  TF = record
    value: Integer;
  end;

const
  C2 = 200;

type
  TG = Integer;
  TH = Integer;

var
  a: TA;
  b: TB;
  c: TC;
  d: TD;
  e: TE;
  f: TF;
  g: TG;
  h: TH;

begin
  a := 1;
  b := 2;
  c := 3;
  d := 4;
  e := 5;
  f.value := 6;
  g := 7;
  h := 8;
  globalVar := C1 + C2;
  
  writeln('TA: ', a);
  writeln('TB: ', b);
  writeln('TC: ', c);
  writeln('TD: ', d);
  writeln('TE: ', e);
  writeln('TF.value: ', f.value);
  writeln('TG: ', g);
  writeln('TH: ', h);
  writeln('globalVar: ', globalVar);
end.
