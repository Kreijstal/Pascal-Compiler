{ FPC Bootstrap Gap: PPAnsiChar type
  FPC's unixutil.pp uses PPAnsiChar (pointer to pointer to AnsiChar).
}
program fpc_ppansichar;

var
  p: PAnsiChar;
  pp: PPAnsiChar;

begin
  p := 'Hello';
  pp := @p;
  writeln('String: ', p);
  writeln('Pointer value is set: ', pp <> nil);
end.
