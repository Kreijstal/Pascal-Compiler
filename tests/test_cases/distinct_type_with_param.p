program distinct_type_with_param;
{ Test parsing type declarations after distinct type with parameter syntax.
  The type 'type AnsiString(CP_UTF8)' is FPC-specific but should not break
  parsing of subsequent type declarations. }

const
  CP_UTF8 = 65001;

type
  TA = Integer;
  UTF8String = type AnsiString(CP_UTF8);
  TB = Integer;

var
  a: TA;
  b: TB;

begin
  a := 1;
  b := 2;
  writeln('TA value: ', a);
  writeln('TB value: ', b);
end.
