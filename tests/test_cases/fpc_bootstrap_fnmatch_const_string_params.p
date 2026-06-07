{$H+}
program fpc_bootstrap_fnmatch_const_string_params;

function MatchesStar(const Pattern, Name: string): boolean;
var
  LenPat, LenName: longint;

  function DoMatch(i, j: longint): boolean;
  var
    Found: boolean;
  begin
    Found := true;
    while Found and (i <= LenPat) do
    begin
      if Pattern[i] = '*' then
      begin
        while Found do
        begin
          inc(i);
          if i > LenPat then
            break;
        end;
        Found := false;
        if i > LenPat then
        begin
          j := LenName;
          Found := true;
        end;
      end
      else
        Found := (j <= LenName) and (Pattern[i] = Name[j]);
      inc(i);
      inc(j);
    end;
    DoMatch := Found and (j > LenName);
  end;

begin
  LenPat := Length(Pattern);
  LenName := Length(Name);
  Writeln('pat-len=', LenPat);
  Writeln('name-len=', LenName);
  MatchesStar := DoMatch(1, 1);
end;

var
  SName, FName: RawByteString;

begin
  if MatchesStar('*', '..') then
    Writeln('literal-match')
  else
    Writeln('literal-miss');

  SName := '*';
  FName := '..';
  if MatchesStar(SName, FName) then
    Writeln('rawbyte-var-match')
  else
    Writeln('rawbyte-var-miss');
end.
