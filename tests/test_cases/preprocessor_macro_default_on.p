program preprocessor_macro_default_on;

{$define MYTYPE:=ShortString}

type
  TMy = MYTYPE;

var
  s: TMy;

begin
  s := 'hi';
  writeln(s);
end.
