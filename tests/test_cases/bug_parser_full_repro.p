program bug_parser_full_repro;

{ objfpc}

type
  ShortString = string[255];
  CodePointer = Pointer;
  PCodePointer = ^CodePointer;
  TObject = class end;
  Exception = class(TObject) end;

var
  WriteErrorsToStdErr: Boolean;
  g_result: Integer;

{}
{-}
Procedure CatchUnhandledException (Obj : TObject; Addr: CodePointer; FrameCount: Longint; Frames: PCodePointer);[public,alias:'FPC_BREAK_UNHANDLED_EXCEPTION'];
Var
  i : longint;
begin
  if WriteErrorsToStdErr then
    i := 1
  else
    i := 2;
  if Obj is exception then
    g_result := i;
end;

Procedure AssertErrorHandler (Const Msg,FN : ShortString;LineNo:longint; TheAddr : pointer);
Var
  S : String;
begin
  If Msg='' then
    S:='Assertion failed'
  else
    S:=Msg;
end;
{}

procedure InitExceptions;
begin
end;

begin
  InitExceptions;
  Writeln('OK');
end.
