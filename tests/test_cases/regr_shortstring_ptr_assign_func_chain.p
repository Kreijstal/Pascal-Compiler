program regr_shortstring_ptr_assign_func_chain;

{$mode objfpc}

uses sysutils;

type
  PAnsiStr = ^AnsiString;

function MakeShortString: ShortString;
begin
  MakeShortString := 'TINY_UNIT';
end;

var
  p: PAnsiStr;
begin
  new(p);
  { This pattern triggered the kgpc_string_assign vs shortstring buffer
    mismatch.  The RHS is a function returning ShortString (passed via
    SRET as a 256-byte buffer with a length byte at offset 0); the LHS
    is a dereferenced pointer to AnsiString.  Without the conversion,
    the runtime treated the shortstring buffer as if it were an
    AnsiString and copied the leading length byte as a character. }
  p^ := MakeShortString;
  writeln('s=[', p^, '] len=', length(p^));
  if p^ = 'TINY_UNIT' then writeln('OK') else writeln('BUG');
  dispose(p);
end.
