unit Classes;

interface

type
  TStringList = class(TObject)
  public
    FItems: array of string;
    FCount: integer;
    FSorted: boolean;
    constructor Create;
    procedure Add(const S: string);
    procedure Delete(Index: integer);
    procedure Sort;
    procedure LoadFromFile(const FileName: string);
    procedure AddDelimitedtext(const S: string; Delimiter: char; Strict: boolean);
    property Count: integer read FCount;
  end;

implementation

constructor TStringList.Create;
begin
  FCount := 0;
end;

procedure TStringList.Add(const S: string);
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := S;
  FCount := FCount + 1;
end;

procedure TStringList.Delete(Index: integer);
var
  i: integer;
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    for i := Index to FCount - 2 do
      FItems[i] := FItems[i + 1];
    FCount := FCount - 1;
    SetLength(FItems, FCount);
  end;
end;

procedure TStringList.Sort;
var
  i, j: integer;
  temp: string;
begin
  for i := 0 to FCount - 2 do
    for j := i + 1 to FCount - 1 do
      if FItems[i] > FItems[j] then
      begin
        temp := FItems[i];
        FItems[i] := FItems[j];
        FItems[j] := temp;
      end;
end;

procedure TStringList.LoadFromFile(const FileName: string);
var
  f: text;
  s: string;
begin
  Assign(f, FileName);
  Reset(f);
  while not EOF(f) do
  begin
    ReadLn(f, s);
    Self.Add(s);
  end;
  Close(f);
end;

procedure TStringList.AddDelimitedtext(const S: string; Delimiter: char; Strict: boolean);
var
  i, start, len: integer;
  sub: string;
  function TrimString(const Str: string): string;
  var
    s, e: integer;
  begin
    s := 1;
    while (s <= Length(Str)) and (Str[s] <= ' ') do s := s + 1;
    e := Length(Str);
    while (e >= s) and (Str[e] <= ' ') do e := e - 1;
    if s > e then TrimString := ''
    else TrimString := Copy(Str, s, e - s + 1);
  end;
begin
  if S = '' then exit;
  start := 1;
  len := Length(S);
  for i := 1 to len do
  begin
    { In non-strict mode, both Delimiter and whitespace act as delimiters }
    if (S[i] = Delimiter) or (not Strict and (S[i] <= ' ')) then
    begin
      sub := Copy(S, start, i - start);
      if not Strict then sub := TrimString(sub);

      { Only add non-empty tokens if not strict }
      if Strict or (Length(sub) > 0) then
        Self.Add(sub);

      start := i + 1;
    end;
  end;

  sub := Copy(S, start, len - start + 1);
  if not Strict then sub := TrimString(sub);
  if Strict or (Length(sub) > 0) then
    Self.Add(sub);
end;

end.
