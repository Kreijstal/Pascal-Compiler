program test_proc_const_tdd;
{
  TDD test: Typed constants with inline procedure types.
  Exercises:
  - Typed constant declaration with procedure type (const X: procedure(...) = nil)
  - Assigned() check on procedural constant
  - Calling procedural constant after assignment
  - Both procedure and function variants
  - Multiple parameter passing modes (var, const, by value)
  
  This pattern is used heavily in FPC's variant support (VarClearProc, 
  VarCopyProc, VarAddRefProc) and must work for system.pp to compile.
}

type
  TVarData = record
    VType: Word;
    VInteger: LongInt;
    VString: String;
  end;

const
  VarClearProc: procedure(var v: TVarData) = nil;
  VarCopyProc: procedure(var d: TVarData; const s: TVarData) = nil;
  VarToStrProc: function(const v: TVarData): String = nil;
  VarFromStrProc: procedure(var v: TVarData; const s: String) = nil;

procedure DoClear(var v: TVarData);
begin
  v.VType := 0;
  v.VInteger := 0;
  v.VString := '';
end;

procedure DoCopy(var d: TVarData; const s: TVarData);
begin
  d.VType := s.VType;
  d.VInteger := s.VInteger;
  d.VString := s.VString;
end;

function DoToStr(const v: TVarData): String;
begin
  DoToStr := v.VString;
end;

procedure DoFromStr(var v: TVarData; const s: String);
begin
  v.VType := 42;
  v.VString := s;
end;

var
  v1, v2: TVarData;
  s: String;
  allOk: Boolean;
begin
  allOk := True;
  
  { Test 1: Unassigned procedural constants should be nil }
  if Assigned(VarClearProc) then begin
    writeln('FAIL: VarClearProc should be nil initially');
    allOk := False;
  end;
  if Assigned(VarCopyProc) then begin
    writeln('FAIL: VarCopyProc should be nil initially');
    allOk := False;
  end;
  if Assigned(VarToStrProc) then begin
    writeln('FAIL: VarToStrProc should be nil initially');
    allOk := False;
  end;
  if Assigned(VarFromStrProc) then begin
    writeln('FAIL: VarFromStrProc should be nil initially');
    allOk := False;
  end;
  
  { Test 2: Assign procedure pointers }
  VarClearProc := @DoClear;
  VarCopyProc := @DoCopy;
  VarToStrProc := @DoToStr;
  VarFromStrProc := @DoFromStr;
  
  { Test 3: Check assigned after assignment }
  if not Assigned(VarClearProc) then begin
    writeln('FAIL: VarClearProc should be assigned');
    allOk := False;
  end;
  if not Assigned(VarCopyProc) then begin
    writeln('FAIL: VarCopyProc should be assigned');
    allOk := False;
  end;
  if not Assigned(VarToStrProc) then begin
    writeln('FAIL: VarToStrProc should be assigned');
    allOk := False;
  end;
  if not Assigned(VarFromStrProc) then begin
    writeln('FAIL: VarFromStrProc should be assigned');
    allOk := False;
  end;
  
  { Test 4: Call through procedural constants }
  v1.VType := 1;
  v1.VInteger := 100;
  v1.VString := 'hello';
  
  { Call VarCopyProc }
  VarCopyProc(v2, v1);
  if v2.VInteger <> 100 then begin
    writeln('FAIL: VarCopyProc did not copy VInteger');
    allOk := False;
  end;
  if v2.VString <> 'hello' then begin
    writeln('FAIL: VarCopyProc did not copy VString');
    allOk := False;
  end;
  
  { Call VarToStrProc (function) }
  s := VarToStrProc(v1);
  if s <> 'hello' then begin
    writeln('FAIL: VarToStrProc returned wrong value');
    allOk := False;
  end;
  
  { Call VarFromStrProc }
  VarFromStrProc(v1, 'world');
  if v1.VType <> 42 then begin
    writeln('FAIL: VarFromStrProc did not set VType');
    allOk := False;
  end;
  if v1.VString <> 'world' then begin
    writeln('FAIL: VarFromStrProc did not set VString');
    allOk := False;
  end;
  
  { Call VarClearProc }
  VarClearProc(v1);
  if v1.VInteger <> 0 then begin
    writeln('FAIL: VarClearProc did not clear VInteger');
    allOk := False;
  end;
  
  if allOk then
    writeln('OK')
  else
    writeln('FAILED');
end.
