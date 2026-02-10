program tdd_fpopen_ambiguous;

{$mode objfpc}

type
  TMode = LongInt;

function fpOpen(path: PAnsiChar; flags: LongInt): LongInt; overload;
begin
  fpOpen := 10;
end;

function fpOpen(path: PAnsiChar; flags: LongInt; mode: TMode): LongInt; overload;
begin
  fpOpen := 20;
end;

function fpOpen(const path: RawByteString; flags: LongInt): LongInt; overload;
begin
  fpOpen := 30;
end;

function fpOpen(path: ShortString; flags: LongInt): LongInt; overload;
begin
  fpOpen := 40;
end;

var
  s: RawByteString;
  res: LongInt;
begin
  s := 'abc';
  res := fpOpen(pointer(s), 7);
  Writeln('fpOpen result=', res);
end.
