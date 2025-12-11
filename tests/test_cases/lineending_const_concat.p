program lineending_const_concat;
{ Repro: kgpc previously rejected LineEnding in const string expressions (needed by FPC sysconst). }
const
  Msg = 'Hello' + LineEnding + 'World';
begin
  writeln(Msg);
end.
