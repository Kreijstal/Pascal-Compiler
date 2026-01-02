program fpc_bootstrap_codepage_aliases;
{$mode objfpc}

{ Bootstrap test: alias normalization using PAnsiChar tables. }
{ FPC: should output latin1, utf-8, 1252 }

type
  TAlias = record
    alias: PAnsiChar;
    canonical: PAnsiChar;
  end;

const
  AliasCount = 5;
  Aliases: array[0..AliasCount - 1] of TAlias = (
    (alias: 'latin1'; canonical: 'latin1'),
    (alias: 'latin-1'; canonical: 'latin1'),
    (alias: 'utf8'; canonical: 'utf-8'),
    (alias: 'koi8r'; canonical: 'koi8-r'),
    (alias: 'iso8859-1'; canonical: 'latin1')
  );

function NormalizeName(const s: AnsiString): AnsiString;
var
  tmp: AnsiString;
  i: Integer;
begin
  tmp := s;
  if (Length(tmp) > 2) and
     ((tmp[1] = 'c') or (tmp[1] = 'C')) and
     ((tmp[2] = 'p') or (tmp[2] = 'P')) then
    tmp := Copy(tmp, 3, Length(tmp) - 2);

  for i := Low(Aliases) to High(Aliases) do
  begin
    if tmp = Aliases[i].alias then
    begin
      Result := AnsiString(Aliases[i].canonical);
      Exit;
    end;
  end;

  Result := tmp;
end;

begin
  writeln(NormalizeName('latin1'));
  writeln(NormalizeName('utf8'));
  writeln(NormalizeName('cp1252'));
end.
