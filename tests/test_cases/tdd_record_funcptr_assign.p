{$mode objfpc}
program test_pansichar_assign;

{ Test: Assigning PAnsiChar result from record function pointer to PAnsiChar Result
  This tests that when a function pointer in a record returns PAnsiChar,
  assigning its result to a function's Result variable of type PAnsiChar works. }

type
  TStrFunc = function(S: PAnsiChar): PAnsiChar;
  
  TManager = record
    StrLowerProc: TStrFunc;
  end;

function MyStrLower(S: PAnsiChar): PAnsiChar;
var
  P: PAnsiChar;
begin
  P := S;
  if P <> nil then
    while P^ <> #0 do
    begin
      if (P^ >= 'A') and (P^ <= 'Z') then
        P^ := Chr(Ord(P^) + 32);
      Inc(P);
    end;
  Result := S;
end;

var
  Manager: TManager;

function AnsiStrLower(Str: PAnsiChar): PAnsiChar;
begin
  { This assignment should work - both are PAnsiChar }
  Result := Manager.StrLowerProc(Str);
end;

var
  Buf: array[0..10] of AnsiChar;
  R: PAnsiChar;
begin
  Manager.StrLowerProc := @MyStrLower;
  Buf[0] := 'H';
  Buf[1] := 'I';
  Buf[2] := #0;
  R := AnsiStrLower(@Buf[0]);
  WriteLn(R);
end.
