program tdd_compiler_intrinsics;
{$mode objfpc}

{ Test FPC compiler intrinsics that KGPC should support }

const
  PathDelim = '/';
  AltPathDelim = '\';

function TestCharConst: Boolean;
{ Test that single-char constants are properly typed as Char }
var
  s: String;
  c: Char;
  lastMatch: Boolean;
begin
  s := '/usr/local/';
  c := s[Length(s)];

  { Char constant comparison in boolean assignment }
  lastMatch := (c = PathDelim);
  Result := lastMatch;
end;

function TestCharConstIndex: Boolean;
{ Test char const comparison with string indexing }
var
  s: String;
begin
  s := '/usr/local/';
  { Direct indexed comparison with char constant }
  if s[Length(s)] = PathDelim then
    Result := True
  else
    Result := False;
end;

function TestCharConstAssign: Char;
{ Test assigning untyped char const to a Char variable }
var
  c: Char;
begin
  c := PathDelim;
  Result := c;
end;

function TestGetFrame: Boolean;
{ Test get_frame compiler intrinsic }
var
  frame: Pointer;
begin
  frame := get_frame;
  Result := (frame <> nil);
end;

function TestGetCallerAddr: Boolean;
{ Test get_caller_addr compiler intrinsic }
var
  addr: Pointer;
begin
  addr := get_caller_addr(get_frame);
  Result := (addr <> nil);
end;

begin
  { Char constant tests }
  if TestCharConst then
    WriteLn('CharConst: PASS')
  else
    WriteLn('CharConst: FAIL');

  if TestCharConstIndex then
    WriteLn('CharConstIndex: PASS')
  else
    WriteLn('CharConstIndex: FAIL');

  if TestCharConstAssign = '/' then
    WriteLn('CharConstAssign: PASS')
  else
    WriteLn('CharConstAssign: FAIL');

  { Compiler intrinsic tests }
  if TestGetFrame then
    WriteLn('GetFrame: PASS')
  else
    WriteLn('GetFrame: FAIL');

  if TestGetCallerAddr then
    WriteLn('GetCallerAddr: PASS')
  else
    WriteLn('GetCallerAddr: FAIL');
end.
