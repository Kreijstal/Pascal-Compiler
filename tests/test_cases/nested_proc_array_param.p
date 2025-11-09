program nested_proc_array_param;
{ Test for nested procedures passing array parameters to procedures at different nesting levels.
  This was causing a segfault due to the static link register being overwritten during
  argument evaluation. }
const
  MaximalAlfa = 20;
type
  TAlfa = array[1..MaximalAlfa] of char;
var
  SymbolNameList: array[-1..10] of Integer;
  CurrentLevel: Integer;
  CrashValue: Integer;

procedure EnterSymbol(CurrentIdentifier: TAlfa);
var
  j: Integer;
begin
  j := SymbolNameList[CurrentLevel];
  CrashValue := j;
end;

procedure Outer;
var
  Local: TAlfa;
  procedure Inner;
  begin
    EnterSymbol(Local);
  end;
begin
  Local[1] := 'A';
  CurrentLevel := -1;
  SymbolNameList[-1] := 123;
  Inner;
end;

begin
  Outer;
  writeln('done ', CrashValue);
end.
