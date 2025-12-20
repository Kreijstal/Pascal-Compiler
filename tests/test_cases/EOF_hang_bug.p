program test_eof_hang_bug;
{
  Unit test to reproduce the EOF hanging bug in KGPC runtime.
  
  This test reproduces the bug where the EOF function in runtime.c
  incorrectly handles the ungetc() return value, causing infinite loops
  and segmentation faults when parsing invalid input.
  
  Bug description:
  - kgpc_text_eof() calls fgetc() to check for EOF
  - Then calls ungetc() to put the character back
  - If ungetc() fails (returns EOF), the function incorrectly reports EOF
  - This causes ReadChar procedures to think they're at EOF when data is still available
  - Result: infinite loops and crashes
  
  Expected behavior: Program should handle invalid input gracefully
  Actual behavior: Program hangs and crashes with segmentation fault
}

{$ifdef fpc}
 {$mode delphi}
{$endif}

const SymPROGRAM = 46;
      TokIdent = 0;
      TokSemi = 18;
      TokPeriod = 19;

var CurrentChar: char;
    CurrentLine: integer;
    CurrentColumn: integer;
    CurrentSymbol: integer;

procedure Error(n: integer);
begin
  Write('Error ', n:1, ': ');
  case n of
    SymPROGRAM: Write('"program" expected');
  end;
  WriteLn(' at line ', CurrentLine:1, ' at column ', CurrentColumn:1);
  Halt(0);
end;

procedure ReadChar;
begin
  if not EOF then begin
    read(CurrentChar);
    CurrentColumn := CurrentColumn + 1;
    if CurrentChar = #10 then begin
      CurrentLine := CurrentLine + 1;
      CurrentColumn := 0;
    end;
  end else begin
    CurrentChar := #0;
  end;
end;

procedure GetSymbol;
var i: integer;
begin
  while (CurrentChar > #0) and (CurrentChar <= ' ') do begin
    ReadChar;
  end;
  if CurrentChar = #0 then begin
    CurrentSymbol := TokPeriod;
    exit;
  end;
  
  // Check for identifier
  if (('a' <= CurrentChar) and (CurrentChar <= 'z')) or 
     (('A' <= CurrentChar) and (CurrentChar <= 'Z')) then begin
    i := 0;
    while ((('a' <= CurrentChar) and (CurrentChar <= 'z')) or 
           (('A' <= CurrentChar) and (CurrentChar <= 'Z')) or 
           (('0' <= CurrentChar) and (CurrentChar <= '9'))) or 
           (CurrentChar = '_') do begin
      ReadChar;
      i := i + 1;
      if i > 20 then break; // Prevent infinite loop
    end;
    CurrentSymbol := TokIdent;
  end else begin
    CurrentSymbol := TokPeriod; // Unknown token
  end;
end;

procedure Expect(s: integer);
begin
  if CurrentSymbol <> s then begin
    Error(s);
  end;
  GetSymbol;
end;

begin
  CurrentLine := 1;
  CurrentColumn := 0;
  ReadChar;
  GetSymbol;
  Expect(SymPROGRAM);
  Expect(TokIdent);
  Expect(TokSemi);
  WriteLn('Parsing successful');
end.
