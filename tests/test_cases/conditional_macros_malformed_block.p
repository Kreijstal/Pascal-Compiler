program ConditionalMacrosMalformed;

begin
{$if defined(KGPC)}
  writeln('hello');
  writeln('still inside conditional');
(* missing ENDIF on purpose *)
end.
