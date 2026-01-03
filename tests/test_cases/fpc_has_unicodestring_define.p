program fpc_has_unicodestring_define;

{$mode objfpc}

const
{$ifdef FPC_HAS_UNICODESTRING}
  HasUnicode = 1;
{$else}
  HasUnicode = 0;
{$endif}

begin
  writeln(HasUnicode);
end.
