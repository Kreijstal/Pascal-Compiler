program fpc_bootstrap_hminus_shortstring_func_return;
{$mode objfpc}
{$H-}
{ Bootstrap blocker: Under {$H-}, function returning string (= ShortString)
  must use shortstring assignment, not AnsiString kgpc_string_assign.
  Without this fix, the returned ShortString is corrupted (contains a pointer
  value instead of actual character data), which causes pp.pas to hang
  in charlength -> utf8codepointlen infinite loop. }

function SimpleReturn: string;
begin
  SimpleReturn := 'ABC';
end;

function ConcatReturn: string;
var
  s: string;
begin
  s := 'AB';
  s := s + 'C';
  ConcatReturn := s;
end;

function LoopReturn(n: longint): string;
var
  s: string;
begin
  s := '';
  while length(s) < n do
    s := s + 'X';
  LoopReturn := s;
end;

function PadEnd(s: string; i: longint): string;
begin
  if length(s) >= i then
    s := s + ' '
  else
    while (length(s) < i) do
      s := s + ' ';
  PadEnd := s;
end;

var
  r: string;
begin
  r := SimpleReturn;
  WriteLn(r);
  WriteLn(Length(r));

  r := ConcatReturn;
  WriteLn(r);
  WriteLn(Length(r));

  r := LoopReturn(5);
  WriteLn(r);
  WriteLn(Length(r));

  r := PadEnd('Hi', 6);
  WriteLn(r);
  WriteLn(Length(r));
end.
