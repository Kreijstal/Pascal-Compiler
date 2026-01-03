program fpc_bootstrap_assign_textrec_cast;
{$mode objfpc}
{ Bootstrap blocker: Assign(TextRec) cast used by RTL text helpers. }

var
  tr: TextRec;

begin
  Assign(Text(tr), '');
  writeln('OK');
end.
