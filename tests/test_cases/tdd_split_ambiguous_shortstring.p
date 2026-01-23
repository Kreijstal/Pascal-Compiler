{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
program tdd_split_ambiguous_shortstring;

type
  TStringSplitOption = (soIgnoreEmpty);
  TStringSplitOptions = set of TStringSplitOption;

  TStringChar = Char;
  TStringType = ShortString;
  TStringArray = array of TStringType;

  TStringHelper = type helper for TStringType
    function Split(const Separators: array of TStringChar; AQuoteStart, AQuoteEnd: TStringChar;
      ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    function Split(const Separators: array of TStringType; AQuoteStart, AQuoteEnd: TStringChar;
      ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    function Split(const Separators: array of TStringType; AQuoteStart, AQuoteEnd: TStringChar;
      Options: TStringSplitOptions): TStringArray; overload;
  end;

function TStringHelper.Split(const Separators: array of TStringChar; AQuoteStart, AQuoteEnd: TStringChar;
  ACount: SizeInt; Options: TStringSplitOptions): TStringArray;
begin
  SetLength(Result, ACount);
end;

function TStringHelper.Split(const Separators: array of TStringType; AQuoteStart, AQuoteEnd: TStringChar;
  ACount: SizeInt; Options: TStringSplitOptions): TStringArray;
begin
  SetLength(Result, ACount + System.Length(Separators));
end;

function TStringHelper.Split(const Separators: array of TStringType; AQuoteStart, AQuoteEnd: TStringChar;
  Options: TStringSplitOptions): TStringArray;
begin
  Result := Split(Separators, AQuoteStart, AQuoteEnd, System.Length(Self) + 1, Options);
end;

var
  s: ShortString;
  parts: TStringArray;

begin
  s := 'a;b';
  parts := s.Split([';'], '"', '"', []);
  WriteLn(Length(parts));
end.
