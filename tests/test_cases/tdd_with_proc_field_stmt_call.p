program tdd_with_proc_field_stmt_call;

type
  TAction = procedure;
  TEntry = record
    Init: TAction;
    Done: TAction;
  end;
  PEntry = ^TEntry;

procedure DoInit;
begin
  WriteLn('init');
end;

procedure DoDone;
begin
  WriteLn('done');
end;

procedure Run(E: PEntry);
begin
  with E^ do
  begin
    if Assigned(Init) then
      Init();
    if Assigned(Done) then
      Done();
  end;
end;

var
  Entry: TEntry;
begin
  Entry.Init := @DoInit;
  Entry.Done := @DoDone;
  Run(@Entry);
end.
