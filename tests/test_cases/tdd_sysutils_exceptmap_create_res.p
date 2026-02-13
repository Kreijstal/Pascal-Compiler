program tdd_sysutils_exceptmap_create_res;

{$mode objfpc}

resourcestring
  SAlpha = 'Alpha';
  SBeta = 'Beta';
  SGamma = 'Gamma';
  SDelta = 'Delta';

type
  RTLString = AnsiString;
  PResStringRec = ^RTLString;

  TMyError = class
  private
    FMessage: AnsiString;
  public
    constructor CreateRes(ResString: PResStringRec);
    property Message: AnsiString read FMessage;
  end;

constructor TMyError.CreateRes(ResString: PResStringRec);
begin
  if ResString <> nil then
    FMessage := ResString^
  else
    FMessage := '';
end;

type
  TMyErrorClass = class of TMyError;
  PExceptMapEntry = ^TExceptMapEntry;
  TExceptMapEntry = record
    code: Byte;
    cls: TMyErrorClass;
    msg: PResStringRec;
  end;

const
  ExceptMap: array[0..3] of TExceptMapEntry = (
    (code: 11; cls: TMyError; msg: @SAlpha),
    (code: 22; cls: TMyError; msg: @SBeta),
    (code: 33; cls: TMyError; msg: @SGamma),
    (code: 44; cls: TMyError; msg: @SDelta)
  );

function FindEntry(Target: Byte): PExceptMapEntry;
var
  i: Integer;
begin
  for i := Low(ExceptMap) to High(ExceptMap) do
    if ExceptMap[i].code = Target then
      begin
        Result := @ExceptMap[i];
        Exit;
      end;
  Result := nil;
end;

function MessageScore(const Msg: string): LongInt;
var
  i: Integer;
  sum: LongInt;
begin
  sum := 0;
  for i := 1 to Length(Msg) do
    sum := sum + (Ord(Msg[i]) * i);
  Result := sum + Length(Msg) * 13;
end;

var
  Entry: PExceptMapEntry;
  E: TMyError;
  Score: LongInt;
  totalCodes: LongInt;
  idx: Integer;
begin
  totalCodes := 0;
  for idx := Low(ExceptMap) to High(ExceptMap) do
    totalCodes := totalCodes + ExceptMap[idx].code;

  Entry := FindEntry(33);
  if Entry = nil then
    begin
      Writeln('Entry missing');
      Halt(1);
    end;

  E := Entry^.cls.CreateRes(Entry^.msg);
  Score := MessageScore(E.Message) + totalCodes;
  Writeln('Score=', Score);
end.
