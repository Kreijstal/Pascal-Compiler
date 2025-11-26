program TestComplex;
{$define VAL := 10}
{$define FLAG}

begin
{$if VAL = 10}
  WriteLn('VAL is 10');
{$endif}

{$if (VAL > 5) AND (VAL < 20)}
  WriteLn('VAL is between 5 and 20');
{$endif}

{$if DEFINED(FLAG) AND (VAL >= 10)}
  WriteLn('FLAG is defined and VAL >= 10');
{$endif}

{$if NOT DEFINED(NONEXISTENT)}
  WriteLn('NONEXISTENT is not defined');
{$endif}

{$if VAL <> 5}
  WriteLn('VAL is not 5');
{$endif}
end.
