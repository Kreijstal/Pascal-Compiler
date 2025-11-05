program BTpcStyleTest;

{ Test BTPC-style declarations that should now parse }

const 
  MaximalIdentifiers=512;
  MaximalAlfa=20;

type 
  TAlfa=array[1..MaximalAlfa] of char;
  
  TIdent=record
    Name:TAlfa;
    Link:integer;
    TypeDefinition:integer;
    Kind:integer;
    Value:integer;
  end;

var 
  CurrentChar:char;
  CurrentSymbol:integer;
  CurrentIdentifer:TAlfa;
  CurrentNumber:integer;
  Identifiers:array[0..MaximalIdentifiers] of TIdent;

function StringCompare(var s1,s2:TAlfa):boolean;
var 
  f:boolean;
  i:integer;
begin
  f:=true;
  i:=1;
  while f and (i<=MaximalAlfa) do begin
    f:=(s1[i]=s2[i]);
    i:=i+1;
  end;
  StringCompare:=f;
end;

procedure Error(n:integer);
begin
  Write('Error ',n:1);
  Halt;
end;

procedure GetSymbol;
begin
  if (('a'<=CurrentChar) and (CurrentChar<='z')) or (('A'<=CurrentChar) and (CurrentChar<='Z')) then begin
    CurrentSymbol:=0; {TokIdent}
  end else if (('0'<=CurrentChar) and (CurrentChar<='9')) then begin
    CurrentSymbol:=1; {TokNumber}
  end else begin
    CurrentSymbol:=-1;
  end;
end;

{ Note: This version doesn't have parameter shadowing issue }
procedure EnterSymbol(SymbolName:TAlfa;k,t:integer);
begin
  Identifiers[0].Name:=SymbolName;
end;

function Position:integer;
begin
  Position:=0;
end;

procedure Compile;
begin
  CurrentSymbol:=0;
  CurrentChar:='a';
  GetSymbol;
  if CurrentSymbol=0 then begin
    EnterSymbol(CurrentIdentifer,0,0);
  end;
end;

begin
  Compile;
  WriteLn('BTPC style test completed');
end.
