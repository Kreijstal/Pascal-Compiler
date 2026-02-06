{$mode objfpc}
unit tdd_top_level_subprograms_after_nested_body;

interface

implementation

type
  TVarType = (vtInteger, vtInt64, vtQWord);
  TVarRec = record
    VType: TVarType;
    VInteger: Longint;
    VInt64: PInt64;
    VQWord: PQWord;
  end;
  TFormatSettings = record
    Dummy: Longint;
  end;
  TFormatString = AnsiString;
  TFormatChar = AnsiChar;

const
  feInvalidFormat = 1;

procedure DoFormatError(Code: Longint; const Fmt: AnsiString);
begin
end;

function Format(const Fmt: AnsiString; const Args: array of TVarRec; const FormatSettings: TFormatSettings): AnsiString;
{$i tdd_top_level_subprograms_after_nested_body.inc}

procedure After;
begin
end;

initialization
  After;
end.
