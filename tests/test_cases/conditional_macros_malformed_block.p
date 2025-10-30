program ConditionalMacrosMalformed;

begin
{$if defined(GPC)}
  writeln('hello');
  writeln('still inside conditional');
(* missing ENDIF on purpose *)
end.
