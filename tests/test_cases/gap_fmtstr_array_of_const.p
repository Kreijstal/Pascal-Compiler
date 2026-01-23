{$mode objfpc}
{$H+}
program gap_fmtstr_array_of_const;

uses
  SysUtils;

var
  s: string;

begin
  FmtStr(s, '%d-%s', [42, 'x']);
  WriteLn(s);
end.
