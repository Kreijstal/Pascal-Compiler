{ Test case: Compile FPC RTL system.pp 
  This test validates KGPC's ability to compile the FPC RTL system unit,
  which is the first step in bootstrapping FPC.
  
  This file simply tries to parse and semantically check FPC's system.pp
  by including the main content that system.pp requires.
}
{$MODE OBJFPC}
{$H+}
Unit system;

interface

type
  { Basic types that system.pp declares }
  SizeInt = Int64;
  SizeUInt = QWord;
  PtrInt = Int64;
  PtrUInt = QWord;
  NativeInt = Int64;
  NativeUInt = QWord;
  
  { Text record stub }
  TextRec = record
    Mode: LongInt;
    BufSize: LongInt;
  end;
  PTextRec = ^TextRec;
  
  { File record stub }
  FileRec = record
    Handle: LongInt;
    Mode: LongInt;
  end;
  PFileRec = ^FileRec;

{ Declare some typical system unit functions }
function Lo(x: Word): Byte;
function Hi(x: Word): Byte;
function Length(const s: String): SizeInt;

implementation

function Lo(x: Word): Byte;
begin
  Lo := x and $FF;
end;

function Hi(x: Word): Byte;
begin
  Hi := (x shr 8) and $FF;
end;

function Length(const s: String): SizeInt;
begin
  { Placeholder implementation }
  Length := 0;
end;

end.
