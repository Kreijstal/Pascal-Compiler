program bug_parser_skip_repro;

type
  EError = class
    constructor Create(Msg: String);
  end;

constructor EError.Create(Msg: String);
begin
end;

function GetAddr: Pointer;
begin
  GetAddr := nil;
end;

{$push}
{$S-}
procedure TestProc;
begin
  Raise EError.Create('Error') at GetAddr, GetAddr;
end;
{$pop}

procedure InitExceptions;
begin
end;

begin
  InitExceptions;
  Writeln('OK');
end.
