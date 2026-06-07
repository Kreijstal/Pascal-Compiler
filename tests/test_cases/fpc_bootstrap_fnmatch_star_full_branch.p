{$H+}
program fpc_bootstrap_fnmatch_star_full_branch;

function MatchesStar(const Pattern, Name: RawByteString): boolean;
var
  LenPat, LenName: longint;

  function DoMatch(i, j: longint): boolean;
  var
    UTF8, Found: boolean;
  begin
    UTF8 := false;
    Found := true;
    while Found and (i <= LenPat) do
    begin
      case Pattern[i] of
        '?':
          Found := j <= LenName;
        '*':
          begin
            while Found do
            begin
              inc(i);
              if i > LenPat then
                break;
              case Pattern[i] of
                '*': ;
                '?':
                  begin
                    if j > LenName then
                      exit(false);
                    inc(j);
                  end;
              else
                Found := false;
              end;
            end;
            Assert((i > LenPat) or ((Pattern[i] <> '*') and (Pattern[i] <> '?')));
            Found := false;
            if (i <= LenPat) then
            begin
              if DoMatch(i + ord(not UTF8), j + ord(not UTF8)) then
              begin
                i := LenPat;
                j := LenName;
                Found := true;
              end
              else
                inc(j);
            end
            else
            begin
              j := LenName;
              Found := true;
            end;
          end;
      else
        Found := (j <= LenName) and (Pattern[i] = Name[j]);
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
