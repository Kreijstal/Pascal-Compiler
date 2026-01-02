program fpc_bootstrap_codepage_search;
{$mode objfpc}

{ Bootstrap test: emulate SysUtils codepage lookup using PAnsiChar table. }
{ FPC: should output 28592 }

type
  TCodePageHashEntry = record
    hash: LongWord;
    name: PAnsiChar;
    value: LongWord;
  end;

const
  TableSize = 6;

var
  Table: array[0..TableSize - 1] of TCodePageHashEntry;

function LowerAsciiChar(c: AnsiChar): AnsiChar;
begin
  if (c >= 'A') and (c <= 'Z') then
    Result := AnsiChar(Ord(c) + 32)
  else
    Result := c;
end;

function HashPChar(p: PAnsiChar): LongWord;
var
  h: LongWord;
  c: AnsiChar;
begin
  h := 0;
  if p = nil then
  begin
    Result := 0;
    Exit;
  end;
  while p^ <> #0 do
  begin
    c := LowerAsciiChar(p^);
    h := (h shl 5) + Ord(c);
    Inc(p);
  end;
  Result := h;
end;

procedure SwapEntries(var a, b: TCodePageHashEntry);
var
  tmp: TCodePageHashEntry;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

procedure InitTable;
var
  i, j: Integer;
begin
  Table[0].name := 'latin1';
  Table[0].value := 28591;
  Table[1].name := 'latin2';
  Table[1].value := 28592;
  Table[2].name := 'koi8-r';
  Table[2].value := 20866;
  Table[3].name := 'koi8-u';
  Table[3].value := 21866;
  Table[4].name := 'utf-8';
  Table[4].value := 65001;
  Table[5].name := 'latin9';
  Table[5].value := 28605;

  for i := Low(Table) to High(Table) do
    Table[i].hash := HashPChar(Table[i].name);

  for i := Low(Table) + 1 to High(Table) do
  begin
    j := i;
    while (j > Low(Table)) and
      ((Table[j - 1].hash > Table[j].hash) or
       ((Table[j - 1].hash = Table[j].hash) and
        (AnsiString(Table[j - 1].name) > AnsiString(Table[j].name)))) do
    begin
      SwapEntries(Table[j - 1], Table[j]);
      Dec(j);
    end;
  end;
end;

function FindCodePage(const name: AnsiString): LongWord;
var
  searchName: AnsiString;
  searchHash, foundHash: LongWord;
  L, H, I: Integer;
begin
  searchName := name;
  searchHash := HashPChar(PAnsiChar(searchName));

  L := Low(Table);
  H := High(Table);
  while L <= H do
  begin
    I := (L + H) shr 1;
    foundHash := Table[I].hash;
    if foundHash = searchHash then
    begin
      while (I > Low(Table)) and (Table[I - 1].hash = foundHash) do
        Dec(I);
      while (I <= High(Table)) and (Table[I].hash = foundHash) do
      begin
        if searchName = Table[I].name then
        begin
          Result := Table[I].value;
          Exit;
        end;
        Inc(I);
      end;
      Break;
    end;
    if searchHash > foundHash then
      L := I + 1
    else
      H := I - 1;
  end;
  Result := 0;
end;

begin
  InitTable;
  writeln(FindCodePage('latin2'));
end.
