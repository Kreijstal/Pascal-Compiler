program overload_class_typecast_arg;
{$mode objfpc}
type
  TStored = class end;
  TProc = class(TStored) end;
  TSym = class end;
procedure vis(p: TProc); begin writeln('PROC'); end;
procedure vis(s: TSym); begin writeln('SYM'); end;
var
  d: TStored;
begin
  d := TProc.Create;
  vis(TProc(d));
end.
