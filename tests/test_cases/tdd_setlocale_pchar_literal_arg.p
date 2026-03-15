program tdd_setlocale_pchar_literal_arg;

uses ctypes;

function setlocale(category: cint; locale: PAnsiChar): PAnsiChar; cdecl; external name 'setlocale';

begin
  setlocale(0, '');
  writeln('ok');
end.
