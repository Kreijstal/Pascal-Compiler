{$mode objfpc}
{$H+}
program fpc_bootstrap_dirent_name_to_string;

type
  TDirentLike = record
    d_name: array[0..15] of Char;
  end;

var
  D: TDirentLike;
  A: AnsiString;
  R: RawByteString;

begin
  D.d_name[0] := 'u';
  D.d_name[1] := 'n';
  D.d_name[2] := 'i';
  D.d_name[3] := 'x';
  D.d_name[4] := #0;
  D.d_name[5] := 'X';

  A := D.d_name;
  R := D.d_name;

  WriteLn(A);
  WriteLn(Length(A));
  WriteLn(R);
  WriteLn(Length(R));
end.
