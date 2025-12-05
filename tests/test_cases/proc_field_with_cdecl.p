program proc_field_with_cdecl;
{ Test that procedure fields with calling conventions are parsed correctly.
  This is required for FPC system.pp compatibility (TEntryInformationOS). }

type
  TMyCallback = procedure(val: longint); cdecl;
  
  TRecWithCallback = record
    callback: procedure(e: longint); cdecl;
    data: Integer;
  end;

  TAfterRec = Integer;

var
  r: TRecWithCallback;
  a: TAfterRec;
  cb: TMyCallback;

begin
  r.data := 42;
  a := 100;
  writeln('data: ', r.data);
  writeln('TAfterRec: ', a);
end.
