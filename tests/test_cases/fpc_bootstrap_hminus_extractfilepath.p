program fpc_bootstrap_hminus_extractfilepath;
{$mode objfpc}
{$H-}

uses
  SysUtils;

var
  param_file: string;
  inputfilepath: string;
  inputfilename: string;

begin
  param_file := 'tests/test_cases/helloworld.p';
  inputfilepath := ExtractFilePath(param_file);
  inputfilename := ExtractFileName(param_file);
  WriteLn(inputfilepath);
  WriteLn(inputfilename);
  WriteLn(inputfilepath + inputfilename);
end.
