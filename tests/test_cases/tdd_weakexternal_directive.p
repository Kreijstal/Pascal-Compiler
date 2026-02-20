program tdd_weakexternal_directive;

{$ifdef unix}
function dladdr(lib: pointer; info: pointer): longint; cdecl; weakexternal LibDL name 'dladdr';
{$endif}

begin
  writeln('ok');
end.
