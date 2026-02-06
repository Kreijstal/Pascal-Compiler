program fpc_regression_float_tostrf;
{$mode objfpc}

uses
  sysutils;

var
  S: string;

begin
  S := FloatToStrF(12.34, ffFixed, 6, 2);
  Writeln(S);
end.
