{$mode objfpc}

uses
  sysutils;

const
  Greeting = 'Hello';
  CmdSep = [' ', #9, #10, #$0D];

type
  TGreetings = array[1..1] of string;

var
  Messages: TGreetings;
  Sample: Char;

begin
  Messages[1] := Greeting;
  Sample := ' ';
  if #9 in CmdSep then
    Sample := #9;

  if Sample in CmdSep then
    Writeln(Messages[1]);
end.
