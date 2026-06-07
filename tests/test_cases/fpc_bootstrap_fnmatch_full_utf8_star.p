{$H+}
program fpc_bootstrap_fnmatch_full_utf8_star;

function MatchesStar(const Pattern, Name: RawByteString): boolean;
var
  LenPat, LenName: longint;

  function NameUtf8CodePointLen(index: longint): longint;
  var
    MaxLookAhead: longint;
  begin
    MaxLookAhead := LenName - Index + 1;
    NameUtf8CodePointLen := abs(Utf8CodePointLen(PAnsiChar(@Name[index]), MaxLookAhead, true));
    if NameUtf8CodePointLen = 0 then
      NameUtf8CodePointLen := MaxLookAhead;
  end;

  procedure GoToLastByteOfUtf8CodePoint(var j: longint);
  begin
    inc(j, NameUtf8CodePointLen(j) - 1);
  end;

  function CompareUtf8CodePoint(var i, j: longint; update_i_j: boolean): Boolean;
  var
    bytes, new_i, new_j: longint;
  begin
    bytes := NameUtf8CodePointLen(j);
    new_i := i;
    new_j := j;
    repeat
      dec(bytes);
      Result :=
        (new_j <= LenName) and
        (new_i <= LenPat) and
        (Pattern[new_i] = Name[new_j]);
      inc(new_i);
      inc(new_j);
    until not Result or (bytes = 0);
    if update_i_j then
      begin
        i := new_i;
        j := new_j;
      end;
  end;

  function DoMatch(i, j: longint): boolean;
  var
    UTF8, Found: boolean;
  begin
    Found := true;
    UTF8 := StringCodePage(Name) = CP_UTF8;
    while Found and (i <= LenPat) do
      begin
        case Pattern[i] of
          '?':
            begin
              Found := j <= LenName;
              if UTF8 then
                GoToLastByteOfUtf8CodePoint(j);
            end;
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
                        if UTF8 then
                          GoToLastByteOfUtf8CodePoint(j);
                        inc(j);
                      end;
                  else
                    Found := false;
                  end;
                end;
              Found := false;
              if i <= LenPat then
                begin
                  repeat
                    if UTF8 then
                      begin
                        while (j <= LenName) and
                              ((Name[j] <> Pattern[i]) or
                               not CompareUtf8CodePoint(i, j, false)) do
                          begin
                            GoToLastByteOfUtf8CodePoint(j);
                            inc(j);
                          end;
                      end
                    else
                      begin
                        while (j <= LenName) and (Name[j] <> Pattern[i]) do
                          inc(j);
                      end;
                    if j < LenName then
                      begin
                        if DoMatch(i + ord(not UTF8), j + ord(not UTF8)) then
                          begin
                            i := LenPat;
                            j := LenName;
                            Found := true;
                            break;
                          end
                        else
                          begin
                            if UTF8 then
                              GoToLastByteOfUtf8CodePoint(j);
                            inc(j);
                          end;
                      end
                    else if j = LenName then
                      begin
                        Found := true;
                        break;
                      end;
                  until j > LenName;
                end
              else
                begin
                  j := LenName;
                  Found := true;
                end;
            end;
        #128..#255:
          begin
            Found := (j <= LenName) and (Pattern[i] = Name[j]);
            if Found and UTF8 then
              Found := CompareUtf8CodePoint(i, j, true);
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
  if MatchesStar('*', 'palmos') then
    Writeln('match')
  else
    Writeln('miss');
end.
