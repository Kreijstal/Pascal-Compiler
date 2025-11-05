{$mode objfpc}
{$ifdef Win32}
  {$define Windows}
{$endif}
{$ifdef Win64}
  {$define Windows}
{$endif}
{$ifdef WinCE}
  {$define Windows}
{$endif}
{$ifdef Windows}
  {$apptype console}
{$endif}

begin
  // Test program without program header but with compiler directives
  writeln('Compiler directives work!');
  writeln('Program without header works!');
end.
