{ The static field FData is declared via {$i field.inc}.  Two include search
  dirs (incdyn / incscalar) provide DIFFERENT definitions of that include:
  one makes FData a dynamic array, the other a plain Integer.  Whether
  SetLength/Length on FData is valid therefore depends entirely on the -I
  search path used to resolve the include, i.e. on the parsed AST, which the
  AST cache must key on the include paths.  See the integration test
  test_ast_cache_include_key.sh. }
unit myunit;
{$mode objfpc}{$H+}
interface
type
  TThing = class
  strict private
    var
      {$i field.inc}
    class function Grow: Integer; static;
  end;
implementation
class function TThing.Grow: Integer;
begin
  SetLength(FData, Length(FData) + 1);
  Result := Length(FData);
end;
end.
