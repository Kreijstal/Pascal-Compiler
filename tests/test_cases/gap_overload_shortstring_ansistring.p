program gap_overload_shortstring_ansistring;

{$H-}

procedure Foo(const S: ShortString); overload;
begin
  WriteLn('short');
end;

procedure Foo(const S: AnsiString); overload;
begin
  WriteLn('ansi');
end;

begin
  Foo('hi');
end.
