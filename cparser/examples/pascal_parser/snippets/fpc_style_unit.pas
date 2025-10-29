Unit rax64int;
interface
uses aasmtai, rax86int;
type
  tx8664intreader = class(tx86intreader)
    actsehdirective: TAsmSehDirective;
    function is_targetdirective(const s:string):boolean;override;
  end;
implementation
uses globtype, cutils;
const
  maxoffset: array[boolean] of aint=(high(dword), 240);
function tx8664intreader.is_targetdirective(const s:string):boolean;
begin
  result:=false;
end;
end.
