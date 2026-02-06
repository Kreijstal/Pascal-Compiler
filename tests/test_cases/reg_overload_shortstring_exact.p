{$mode objfpc}
program reg_overload_shortstring_exact;

function Pick(const s: ShortString): Integer; overload;
begin
  Pick := 1;
end;

function Pick(const s: string): Integer; overload;
begin
  Pick := 2;
end;

function Pick(const s: UnicodeString): Integer; overload;
begin
  Pick := 3;
end;

var
  s: ShortString;
begin
  s := 'AbC';
  Writeln(Pick(s));
end.
