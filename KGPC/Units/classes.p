unit Classes;

interface

type
  TStrings = class(TObject)
  public
    FDelimiter: Char;
    FQuoteChar: Char;
    FStrictDelimiter: Boolean;
    constructor Create;
    procedure Add(const S: string); virtual;
    procedure AddDelimitedText(const S: string; ADelimiter: Char; AStrictDelimiter: Boolean); overload;
    procedure AddDelimitedText(const S: string); overload;
  end;

  TStringList = class(TStrings)
  public
    FItems: array of string;
    FCount: integer;
    FSorted: boolean;
    constructor Create;
    procedure Add(const S: string); override;
    procedure AddDelimitedText(const S: string; ADelimiter: Char; AStrictDelimiter: Boolean); overload;
    procedure AddDelimitedText(const S: string); overload;
    procedure Delete(Index: integer);
    procedure Sort;
    procedure LoadFromFile(const FileName: string);
    property Count: integer read FCount;
  end;

implementation

procedure TStrings_AddDelimitedText_Helper(List: TStrings; const S: string; ADelimiter: Char; AStrictDelimiter: Boolean);
var
  len, i, j: integer;
  aNotFirst: boolean;
  quoted: string;
  unescaped: string;
  idx: integer;
  isQuoted: boolean;
begin
  i := 1;
  j := 1;
  aNotFirst := False;
  len := Length(S);
  if len = 0 then
  begin
    List.Add('');
    exit;
  end;
  while i <= len do
  begin
    if aNotFirst and (i <= len) and (S[i] = ADelimiter) then
      Inc(i);
    if not AStrictDelimiter then
      while (i <= len) and (Ord(S[i]) <= Ord(' ')) do
        Inc(i);
    if i > len then
    begin
      if aNotFirst then
        List.Add('');
    end
    else
    begin
      isQuoted := (S[i] = List.FQuoteChar) and (List.FQuoteChar <> #0);
      if isQuoted then
      begin
        j := i + 1;
        while (j <= len) and
              ((S[j] <> List.FQuoteChar) or
              ((j + 1 <= len) and (S[j + 1] = List.FQuoteChar))) do
        begin
          if (S[j] = List.FQuoteChar) and (j + 1 <= len) and (S[j + 1] = List.FQuoteChar) then
            Inc(j, 2)
          else
            Inc(j);
        end;
        quoted := Copy(S, i + 1, j - i - 1);
        unescaped := '';
        idx := 1;
        while idx <= Length(quoted) do
        begin
          if (quoted[idx] = List.FQuoteChar) and (idx < Length(quoted)) and
             (quoted[idx + 1] = List.FQuoteChar) then
          begin
            unescaped := unescaped + List.FQuoteChar;
            Inc(idx, 2);
          end
          else
          begin
            unescaped := unescaped + quoted[idx];
            Inc(idx);
          end;
        end;
        List.Add(unescaped);
        i := j + 1;
      end
      else
      begin
        j := i;
        while (j <= len) and
              (AStrictDelimiter or (Ord(S[j]) > Ord(' '))) and
              (S[j] <> ADelimiter) do
          Inc(j);
        List.Add(Copy(S, i, j - i));
        i := j;
      end;
    end;
    if not AStrictDelimiter then
      while (i <= len) and (Ord(S[i]) <= Ord(' ')) do
        Inc(i);
    aNotFirst := True;
  end;
end;

constructor TStrings.Create;
begin
  FDelimiter := ',';
  FQuoteChar := '"';
  FStrictDelimiter := False;
end;

procedure TStrings.Add(const S: string);
begin
end;

procedure TStrings.AddDelimitedText(const S: string; ADelimiter: Char; AStrictDelimiter: Boolean);
begin
  TStrings_AddDelimitedText_Helper(Self, S, ADelimiter, AStrictDelimiter);
end;

procedure TStrings.AddDelimitedText(const S: string);
begin
  TStrings_AddDelimitedText_Helper(Self, S, FDelimiter, FStrictDelimiter);
end;

constructor TStringList.Create;
begin
  inherited Create;
  FCount := 0;
end;

procedure TStringList.Add(const S: string);
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := S;
  FCount := FCount + 1;
end;

procedure TStringList.AddDelimitedText(const S: string; ADelimiter: Char; AStrictDelimiter: Boolean);
begin
  inherited AddDelimitedText(S, ADelimiter, AStrictDelimiter);
end;

procedure TStringList.AddDelimitedText(const S: string);
begin
  inherited AddDelimitedText(S);
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

end.
