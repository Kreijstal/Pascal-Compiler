{ Test pointer type alias chains: PUInt16 = PWord where PWord = ^Word }
program fpc_pointer_alias_chain;
{$mode objfpc}
type
  PWord = ^Word;
  PUInt16 = PWord;
var
  val: Word;
  p: PUInt16;
begin
  val := 12345;
  p := @val;
  WriteLn(p^);
end.
