program tdd_shortstring_concat_funcresult;
{$mode objfpc}{$H-}
{ Regression: a long shortstring (+) chain containing function-call results
  must not clobber operands.  Under register pressure the codegen allocator
  could hand the concat's RHS the same register it then reloads the LHS into,
  producing concat(LHS, LHS) and dropping intermediate operands.

  This mirrors compiler/comphook.pas's error-position formatting
    hs := currentsource+'('+tostr(line)+','+tostr(col)+') '+hs+' '+s;
    hs := currentsourcepath+hs;
  which produced corrupted messages like
    "dir/system.pp(634system.pp(6341) Error: msg"
  in KGPC-bootstrapped FPC, derailing the asm reader. }

function tostr(i: longint): string;
begin
  str(i, tostr);
end;

var
  src, path, hs, s, r: string;
begin
  src := 'system.pp';
  path := 'dir/';
  hs := 'Error:';
  s := 'msg';
  r := src + '(' + tostr(634) + ',' + tostr(1) + ') ' + hs + ' ' + s;
  r := path + r;
  writeln(r);
end.
