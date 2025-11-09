program StringCopyLoopStuck;
{
  Regression test: copying a fixed-size char array via assignment must copy
  every element. Gwinn GPC currently writes only the first byte, so the
  sentinel-based search loop never terminates.
}

const
  MaxLen = 4;
  MaxIds = 4;

type
  TAlfa = packed array[1..MaxLen] of char;
  TIdentifier = record
    Name: TAlfa;
    Link: integer;
  end;

var
  Identifiers: array[0..MaxIds] of TIdentifier;
  CurrentIdentifer: TAlfa;
  SymbolNameList: array[0..0] of integer;
  CurrentLevel: integer;
  IdentifierPosition: integer;

function StringCompare(var a, b: TAlfa): boolean;
var
  i: integer;
  equal: boolean;
begin
  equal := true;
  i := 1;
  while equal and (i <= MaxLen) do
  begin
    equal := a[i] = b[i];
    i := i + 1;
  end;
  StringCompare := equal;
end;

procedure SetIdent(var target: TAlfa; const value: string);
var
  i: integer;
begin
  for i := 1 to MaxLen do
    target[i] := ' ';
  for i := 1 to length(value) do
    target[i] := value[i];
end;

procedure EnterSymbol;
var
  j: integer;
begin
  IdentifierPosition := IdentifierPosition + 1;
  Identifiers[0].Name := CurrentIdentifer;
  j := SymbolNameList[CurrentLevel];
  while not StringCompare(Identifiers[j].Name, CurrentIdentifer) do
    j := Identifiers[j].Link;
  SymbolNameList[CurrentLevel] := IdentifierPosition;
  Identifiers[IdentifierPosition].Name := CurrentIdentifer;
end;

begin
  CurrentLevel := 0;
  SymbolNameList[0] := 0;
  IdentifierPosition := 0;

  SetIdent(CurrentIdentifer, 'AAAA');
  EnterSymbol;
  Identifiers[1].Link := 0;

  SetIdent(CurrentIdentifer, 'BBBB');
  EnterSymbol;

  writeln('string copy works');
end.
