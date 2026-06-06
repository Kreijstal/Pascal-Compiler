program fpc_bootstrap_fnmatch_star_length;

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
      end;
      inc(i);
      inc(j);
    end;
    DoMatch := Found and (j > LenName);
  end;

begin
  LenPat := Length(Pattern);
  LenName := Length(Name);
  MatchesStar := DoMatch(1, 1);
end;

begin
  if MatchesStar('*', 'symbian') then
    writeln('match')
  else
    writeln('miss');
end.
