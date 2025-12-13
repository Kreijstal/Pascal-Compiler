{ Test forward procedure references in typed constants }
{ FPC allows referencing forward-declared procedures in typed constants }
program forward_proc_const_ref;

type
  TProc = procedure;

procedure MyProc; forward;

const
  MyProcRef: TProc = @MyProc;

procedure MyProc;
begin
  WriteLn('MyProc called');
end;

begin
  MyProcRef();
end.
