program fpc_bootstrap_textrec_filerec_cast;

uses
  SysUtils;

var
  t: Text;
  f: file;
  fname: string;
  textIsOutput: boolean;
  fileIsOutput: boolean;

begin
  fname := 'kgpc_textrec_tmp.dat';

  Assign(t, fname);
  Rewrite(t);
  textIsOutput := TextRec(t).Mode = fmOutput;
  Close(t);

  Assign(f, fname);
  Rewrite(f, 1);
  fileIsOutput := FileRec(f).Mode = fmOutput;
  Close(f);

  if FileExists(fname) then
    DeleteFile(fname);

  if textIsOutput then
    Writeln('text=output')
  else
    Writeln('text=other');

  if fileIsOutput then
    Writeln('file=output')
  else
    Writeln('file=other');
end.
