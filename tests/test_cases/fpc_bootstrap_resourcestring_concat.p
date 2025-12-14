{ Test: resourcestring with string concatenation }
{ From FPC sysconst.pp - many resourcestrings use concatenation }
{ Example: SInvalidVarOpWithHResultWithPrefix = 'Invalid variant operation (%s%.8x)'+LineEnding+'%s' }
program fpc_bootstrap_resourcestring_concat;
{$mode objfpc}

resourcestring
  SSimple = 'Simple message';
  SConcat = 'Part one' + ' ' + 'Part two';
  SWithLineEnding = 'Line one' + LineEnding + 'Line two';

begin
  WriteLn(SSimple);
  WriteLn(SConcat);
  WriteLn(SWithLineEnding);
  WriteLn('Done');
end.
