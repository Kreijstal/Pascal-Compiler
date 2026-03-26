{$H+}
program TDDClassStaticViaClassVar;

type
  TNodeUtils = class
    class function has_init_list: Boolean; static;
  end;

  TNodeUtilsClass = class of TNodeUtils;

var
  cnodeutils: TNodeUtilsClass;

class function TNodeUtils.has_init_list: Boolean;
begin
  has_init_list := True;
end;

begin
  cnodeutils := TNodeUtils;
  if cnodeutils.has_init_list then
    Writeln('ok');
end.
