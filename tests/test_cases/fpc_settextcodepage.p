program TestSetTextCodePage;
{ Test that SetTextCodePage procedure is available.
  This is required for FPC bootstrap - objpas.pp uses this procedure. }
var
  t: Text;
begin
  Assign(t, 'utf8_output.txt');
  SetTextCodePage(t, 65001);  { UTF-8 code page }
  Rewrite(t);
  WriteLn(t, 'SetTextCodePage works');
  Close(t);
  WriteLn('OK');
end.
