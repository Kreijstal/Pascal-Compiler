{ Test case for circular AST reference in const section }
{ This test should compile without hanging }

const type TAlfa=array of char;TIdent=record
      Name:TAlfa;Link:integer;Kind:integer     end;
var CurrentChar:char;
    CurrentIdentifer:TAlfa;CurrentString:array of char;
    CurrentStringLength:integer;
    Keywords:TAlfa;
    CurrentLevel:boolean;
    SymbolNameList:array of integer;Identifiers:array of TIdent;function StringCompare(var s1,s2:TAlfa):boolean;
beginend;procedure Error(n:integer);begin
 Halt;end;
function ReadNumber:integer;
var Num:integer;
begin
 begin
  Num := 0;
 end;
 ReadNumber := Num;
end;procedure GetSymbol;var k,s:boolean;begin
 if CurrentChar<='Z' then begin  while CurrentChar='_' do begin;;CurrentIdentifer[k]:=' 'end;begin
   if StringCompare(Keywords,CurrentIdentifer) then begin
    if CurrentChar=''' then begin
     CurrentString[CurrentStringLength]:=chr(ReadNumber);
    end;
   end;
  end; end else if CurrentChar=';' then begin
end
end
procedure EnterSymbol;var j:integer;begin
 Identifiers[0].Name:=CurrentIdentifer;j:=SymbolNameList[CurrentLevel];
 { Removed the while loop that references IdFUNC which doesn't exist }
 begin
  if Identifiers[j].Kind<>0 then begin
  end;
 end;
end;
begin
end.
