{ Test FileAge function from SysUtils }
program fpc_fileage;
{$mode objfpc}
uses SysUtils;
var
  age: LongInt;
begin
  { Test with a file that doesn't exist - should return -1 }
  age := FileAge('/tmp/nonexistent_file_kgpc_test_12345.xyz');
  WriteLn(age);
end.
