program MinGwinnDuplicateForward;
{
  Minimal reproduction for the Gwinn Pascal compiler hang.
  The duplicate identifier handling path in EnterSymbol uses
  StringCompare with var parameters. The generated code never
  exits the search loop, so the executable spins forever.
}

const
  MaximalAlfa = 20;
  MaximalIdentifiers = 16;
  MaxLevels = 1;
  OPJmp = 10;

type
  TAlfa = packed array[1..MaximalAlfa] of char;

  TIdentifier = record
    Name: TAlfa;
    Link: integer;
    TypeDefinition: integer;
    Kind: integer;
    FunctionAddress: integer;
  end;

var
  Identifiers: array[0..MaximalIdentifiers] of TIdentifier;
  SymbolNameList: array[0..MaxLevels] of integer;
  CurrentIdentifer: TAlfa;
  CurrentLevel: integer;
  IdentifierPosition: integer;
  FunctionDeclarationIndex: integer;
  Code: array[0..32] of integer;
  CodePosition: integer;

procedure Error(code: integer);
begin
  writeln('Error ', code);
  halt(1);
end;

function StringCompare(var a, b: TAlfa): boolean;
var
  i: integer;
  equal: boolean;
begin
  equal := true;
  i := 1;
  while equal and (i <= MaximalAlfa) do
  begin
    equal := a[i] = b[i];
    i := i + 1;
  end;
  StringCompare := equal;
end;

procedure EnterSymbol(CurrentIdentifer:TAlfa;k,t:integer);
var j:integer;
begin
  if IdentifierPosition=MaximalIdentifiers then
    Error(103);
  IdentifierPosition:=IdentifierPosition+1;
  Identifiers[0].Name:=CurrentIdentifer;
  j:=SymbolNameList[CurrentLevel];
  while not StringCompare(Identifiers[j].Name,CurrentIdentifer) do
    j:=Identifiers[j].Link;
  if j<>0 then
  begin
    if Identifiers[j].Kind<>2 {IdFUNC} then
      Error(104);
    if (Code[Identifiers[j].FunctionAddress]<>OPJmp) or
       (Code[Identifiers[j].FunctionAddress+1]>0) then
      Error(105);
    Identifiers[j].Name[1]:='$';
    Code[Identifiers[j].FunctionAddress+1]:=CodePosition;
    FunctionDeclarationIndex:=j;
  end;
  Identifiers[IdentifierPosition].Name:=CurrentIdentifer;
  Identifiers[IdentifierPosition].Link:=SymbolNameList[CurrentLevel];
  Identifiers[IdentifierPosition].TypeDefinition:=t;
  Identifiers[IdentifierPosition].Kind:=k;
  SymbolNameList[CurrentLevel]:=IdentifierPosition;
end;

procedure SetIdent(var a: TAlfa; s: string);
var
  i, L: integer;
begin
  for i := 1 to MaximalAlfa do
    a[i] := ' ';
  L := length(s);
  if L > MaximalAlfa then
    L := MaximalAlfa;
  for i := 1 to L do
    a[i] := s[i];
end;

begin
  CurrentLevel := 0;
  SymbolNameList[0] := 0;
  IdentifierPosition := 0;
  CodePosition := 1;

  { First symbol }
  SetIdent(CurrentIdentifer, 'FOO');
  EnterSymbol(CurrentIdentifer, 1, 0);

  { Fake forward function to trigger j<>0 path }
  SetIdent(Identifiers[1].Name, 'BAR');
  Identifiers[1].Kind := 2; { IdFUNC }
  Identifiers[1].FunctionAddress := 10;
  SymbolNameList[0] := 1;
  Code[10] := OPJmp;
  Code[11] := -1;

  { Second insert with same name to go through duplicate-handling logic }
  SetIdent(CurrentIdentifer, 'BAR');
  EnterSymbol(CurrentIdentifer, 1, 0);

  writeln('done2');
end.
