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
  i, start: integer;
  sub: string;
begin
  start := 1;
  for i := 1 to Length(S) do
  begin
    if S[i] = Delimiter then
    begin
      sub := Copy(S, start, i - start);
      Self.Add(sub);
      start := i + 1;
    end;
  end;
  if start <= Length(S) then
    Self.Add(Copy(S, start, Length(S) - start + 1));
end;

end.
