unit SysUtils;

interface

uses
    ctypes;

type
    TDateTime = System.TDateTime;
    AnsiString = string;
    Uint64 = cuint64;

    // Added for Unix support
    AnsiChar = Char;
    PAnsiChar = ^AnsiChar;
    PChar = PAnsiChar;
    SmallInt = -32768..32767;
    Word = 0..65535;
    LongWord = 0..4294967295;
    Byte = 0..255;
    TBytes = array of Byte;

    Exception = class
    private
        FMessage: AnsiString;
    public
        constructor Create(const Msg: AnsiString);
        constructor CreateFmt(const Msg: AnsiString; const Args: array of const);
        property Message: AnsiString read FMessage;
    end;

    TEncoding = class
    public
        class function UTF8: TEncoding; static;
        class function ANSI: TEncoding; static;
        class function Default: TEncoding; static;
        class function GetSystemEncoding: TEncoding; static;
        class property SystemEncoding: TEncoding read GetSystemEncoding;
        function GetBytes(const S: AnsiString): TBytes; virtual;
        function GetString(const Bytes: TBytes): AnsiString; virtual;
        function GetString(const Bytes: TBytes; Index, Count: Integer): AnsiString; overload; virtual;
        function GetAnsiString(const Bytes: TBytes): AnsiString; virtual;
        function GetAnsiString(const Bytes: TBytes; Index, Count: Integer): AnsiString; overload; virtual;
    end;

    TReplaceFlag = (rfReplaceAll, rfIgnoreCase);
    TReplaceFlags = set of TReplaceFlag;

const
    PathDelim = '/';
    AltPathDelim = '\';
    CP_UTF8 = 65001;
    
    AlphaNum: set of char = ['A'..'Z', 'a'..'z', '0'..'9'];

procedure Sleep(milliseconds: integer);
function GetTickCount64: longint;
function Now: TDateTime;
function IntToStr(value: longint): AnsiString;
function UpperCase(const S: AnsiString): AnsiString;
function LowerCase(const S: AnsiString): AnsiString;
function Trim(const S: AnsiString): AnsiString;
function TrimLeft(const S: AnsiString): AnsiString;
function TrimRight(const S: AnsiString): AnsiString;
function AnsiUpperCase(const S: AnsiString): AnsiString;
function AnsiLowerCase(const S: AnsiString): AnsiString;
function CompareText(const S1, S2: AnsiString): Integer;
function SameText(const S1, S2: AnsiString): Boolean;
function StringReplace(const S, OldPattern, NewPattern: AnsiString): AnsiString;
function StringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
function Pos(Substr: AnsiString; S: AnsiString): integer;
function FormatDateTime(const FormatStr: string; DateTime: TDateTime): AnsiString;
function DateTimeToStr(DateTime: TDateTime): AnsiString;
function TimeToStr(DateTime: TDateTime): AnsiString;
function UnixToDateTime(UnixTime: Int64): TDateTime;
function Format(const Fmt: string; const Args: array of const): string;
function StrPas(P: PAnsiChar): AnsiString;
function StrPas(P: PChar): AnsiString;
function StrLen(P: PAnsiChar): SizeInt;
function StrPos(Str1, Str2: PAnsiChar): PAnsiChar;
function StrRScan(P: PAnsiChar; C: AnsiChar): PAnsiChar;
function FloatToStr(Value: Real): AnsiString;
function StrToInt(const S: AnsiString): longint;
function StrToFloat(const S: AnsiString): Real;
function TryStrToFloat(const S: AnsiString; out Value: Real): Boolean;
function TryStrToInt(const S: AnsiString; out Value: Longint): Boolean;
function ExtractFilePath(const FileName: AnsiString): AnsiString;
function ExtractFileName(const FileName: AnsiString): AnsiString;
function ExtractFileExt(const FileName: AnsiString): AnsiString;
function ChangeFileExt(const FileName, Extension: AnsiString): AnsiString;
function IncludeTrailingPathDelimiter(const Dir: AnsiString): AnsiString;
function ExcludeTrailingPathDelimiter(const Dir: AnsiString): AnsiString;
function FileExists(const FileName: AnsiString): Boolean;
function DeleteFile(const FileName: AnsiString): Boolean;
function DirectoryExists(const DirName: AnsiString): Boolean;
function RenameFile(const OldName, NewName: AnsiString): Boolean;
function GetCurrentDir: AnsiString;
function SetCurrentDir(const Dir: AnsiString): Boolean;
function GetEnvironmentVariable(const Name: AnsiString): AnsiString;
function SetEnvironmentVariable(const Name, Value: AnsiString): Boolean;
function UnsetEnvironmentVariable(const Name: AnsiString): Boolean;
function GetProcessID: Longint;
function LoadLibrary(const Name: AnsiString): NativeUInt;
function GetProcedureAddress(LibHandle: NativeUInt; const ProcName: AnsiString): NativeUInt;
function FreeLibrary(LibHandle: NativeUInt): Boolean;
procedure SetString(out S: AnsiString; Buffer: PAnsiChar; Len: Integer);
function FileDateToDateTime(FileDate: LongInt): TDateTime;
function StringToGUID(const S: AnsiString): TGUID;

{ Generic procedure to free an object and set its reference to nil }
procedure FreeAndNil(var Obj: Pointer);

implementation

function kgpc_format(fmt: AnsiString; args: Pointer; count: NativeUInt): AnsiString; external;
function kgpc_string_to_int(text: PChar; out value: Integer): Integer; external;
function kgpc_string_to_real(text: PChar; out value: Double): Integer; external;
function kgpc_file_exists(path: PChar): Integer; external;
function kgpc_directory_exists(path: PChar): Integer; external;
procedure kgpc_sleep_ms(milliseconds: integer); external;
function kgpc_get_tick_count64: NativeUInt; external;
function kgpc_now: Double; external;
function kgpc_format_datetime(format: PChar; datetime: Double): AnsiString; external;
function kgpc_string_compare(lhs: PChar; rhs: PChar): Integer; external;
function kgpc_string_pos(SubStr: PChar; S: PChar): Integer; external;
function kgpc_extract_file_path(path: PChar): AnsiString; external;
function kgpc_extract_file_name(path: PChar): AnsiString; external;
function kgpc_extract_file_ext(path: PChar): AnsiString; external;
function kgpc_change_file_ext(path: PChar; extension: PChar): AnsiString; external;
function kgpc_exclude_trailing_path_delim(path: PChar): AnsiString; external;
function kgpc_delete_file(path: PChar): Integer; external;
function kgpc_file_rename(old_path: PChar; new_path: PChar): Integer; external;
function kgpc_get_current_dir: AnsiString; external;
function kgpc_set_current_dir(path: PChar): Integer; external;
function kgpc_get_environment_variable(name: PChar): AnsiString; external;
function kgpc_set_environment_variable(name: PChar; value: PChar): Integer; external;
function kgpc_unset_environment_variable(name: PChar): Integer; external;
function kgpc_get_process_id: NativeUInt; external;
function kgpc_load_library(path: PChar): NativeUInt; external;
function kgpc_get_proc_address(handle: NativeUInt; symbol: PChar): NativeUInt; external;
function kgpc_free_library(handle: NativeUInt): Integer; external;
function kgpc_strpas(p: PAnsiChar): AnsiString; external;

function ToPChar(const S: AnsiString): PChar;
begin
    if S = '' then
        Result := nil
    else
        Result := @S[1];
end;

function StrPas(P: PAnsiChar): AnsiString;
begin
    if P = nil then
        StrPas := ''
    else
        StrPas := kgpc_strpas(P);
end;

function StrPas(P: PChar): AnsiString;
begin
    if P = nil then
        StrPas := ''
    else
        StrPas := kgpc_strpas(PAnsiChar(P));
end;

function StrLen(P: PAnsiChar): SizeInt;
var
    Len: SizeInt;
begin
    if P = nil then
    begin
        StrLen := 0;
        exit;
    end;
    Len := 0;
    while P^ <> #0 do
    begin
        Len := Len + 1;
        P := P + 1;
    end;
    StrLen := Len;
end;

function StrPos(Str1, Str2: PAnsiChar): PAnsiChar;
var
    S1: AnsiString;
    S2: AnsiString;
    Index: SizeInt;
begin
    if (Str1 = nil) or (Str2 = nil) then
    begin
        StrPos := nil;
        exit;
    end;
    S1 := StrPas(Str1);
    S2 := StrPas(Str2);
    Index := Pos(S2, S1);
    if Index <= 0 then
        StrPos := nil
    else
        StrPos := PAnsiChar(PtrUInt(Str1) + PtrUInt(Index - 1));
end;

function StrRScan(P: PAnsiChar; C: AnsiChar): PAnsiChar;
var
    Last: PAnsiChar;
begin
    if P = nil then
    begin
        StrRScan := nil;
        exit;
    end;
    Last := nil;
    while P^ <> #0 do
    begin
        if P^ = C then
            Last := P;
        P := P + 1;
    end;
    if C = #0 then
        Last := P;
    StrRScan := Last;
end;


var
    EncodingUtf8Instance: TEncoding;
    EncodingAnsiInstance: TEncoding;
    EncodingDefaultInstance: TEncoding;

class function TEncoding.UTF8: TEncoding;
begin
    if EncodingUtf8Instance = nil then
        EncodingUtf8Instance := TEncoding.Create;
    UTF8 := EncodingUtf8Instance;
end;

class function TEncoding.ANSI: TEncoding;
begin
    if EncodingAnsiInstance = nil then
        EncodingAnsiInstance := TEncoding.Create;
    ANSI := EncodingAnsiInstance;
end;

class function TEncoding.Default: TEncoding;
begin
    if EncodingDefaultInstance = nil then
        EncodingDefaultInstance := TEncoding.UTF8;
    Default := EncodingDefaultInstance;
end;

class function TEncoding.GetSystemEncoding: TEncoding;
begin
    if EncodingDefaultInstance = nil then
        EncodingDefaultInstance := TEncoding.UTF8;
    GetSystemEncoding := EncodingDefaultInstance;
end;

function TEncoding.GetBytes(const S: AnsiString): TBytes;
var
    bytes: TBytes;
    i: Integer;
begin
    SetLength(bytes, Length(S));
    for i := 1 to Length(S) do
        bytes[i - 1] := Ord(S[i]) and $FF;
    GetBytes := bytes;
end;

function TEncoding.GetString(const Bytes: TBytes): AnsiString;
var
    text: AnsiString;
    i: Integer;
begin
    SetLength(text, Length(Bytes));
    for i := 0 to Length(Bytes) - 1 do
        text[i + 1] := Char(Bytes[i]);
    GetString := text;
end;

function TEncoding.GetString(const Bytes: TBytes; Index, Count: Integer): AnsiString;
var
    text: AnsiString;
    i: Integer;
    max_count: Integer;
begin
    if Index < 0 then
        Index := 0;
    if Count < 0 then
        Count := 0;
    max_count := Length(Bytes) - Index;
    if Count > max_count then
        Count := max_count;
    SetLength(text, Count);
    for i := 0 to Count - 1 do
        text[i + 1] := Char(Bytes[Index + i]);
    GetString := text;
end;

function TEncoding.GetAnsiString(const Bytes: TBytes): AnsiString;
var
    text: AnsiString;
    i: Integer;
begin
    SetLength(text, Length(Bytes));
    for i := 0 to Length(Bytes) - 1 do
        text[i + 1] := Char(Bytes[i]);
    GetAnsiString := text;
end;

function TEncoding.GetAnsiString(const Bytes: TBytes; Index, Count: Integer): AnsiString;
var
    text: AnsiString;
    i: Integer;
    max_count: Integer;
begin
    if Index < 0 then
        Index := 0;
    if Count < 0 then
        Count := 0;
    max_count := Length(Bytes) - Index;
    if Count > max_count then
        Count := max_count;
    SetLength(text, Count);
    for i := 0 to Count - 1 do
        text[i + 1] := Char(Bytes[Index + i]);
    GetAnsiString := text;
end;

procedure Sleep(milliseconds: integer);
begin
    kgpc_sleep_ms(milliseconds);
end;

function GetTickCount64: longint;
begin
    GetTickCount64 := kgpc_get_tick_count64();
end;

function Now: TDateTime;
begin
    Now := kgpc_now();
end;

function DigitToString(Value: longint): AnsiString;
var
    DigitValue: longint;
begin
    if Value < 0 then
        DigitValue := 0
    else if Value > 9 then
        DigitValue := 9
    else
        DigitValue := Value;

    DigitToString := Chr(Ord('0') + DigitValue);
end;

function IntToStr(value: longint): AnsiString;
var
    remainder: longint;
    working: longint;
    prefix: AnsiString;
    digits: AnsiString;
begin
    if value = 0 then
    begin
        IntToStr := '0';
        exit;
    end;

    if value < 0 then
    begin
        working := -value;
        prefix := '-';
    end
    else
    begin
        working := value;
        prefix := '';
    end;

    digits := '';
    while working > 0 do
    begin
        remainder := working mod 10;
        digits := DigitToString(remainder) + digits;
        working := working div 10;
    end;

    IntToStr := prefix + digits;
end;

function UpperCase(const S: AnsiString): AnsiString;
var
    i: integer;
    result: AnsiString;
begin
    result := '';
    for i := 1 to Length(S) do
    begin
        if (S[i] >= 'a') and (S[i] <= 'z') then
            result := result + Chr(Ord(S[i]) - 32)
        else
            result := result + S[i];
    end;
    UpperCase := result;
end;

function LowerCase(const S: AnsiString): AnsiString;
var
    i: integer;
    result: AnsiString;
begin
    result := '';
    for i := 1 to Length(S) do
    begin
        if (S[i] >= 'A') and (S[i] <= 'Z') then
            result := result + Chr(Ord(S[i]) + 32)
        else
            result := result + S[i];
    end;
    LowerCase := result;
end;

function Trim(const S: AnsiString): AnsiString;
var
    start, finish: integer;
begin
    start := 1;
    while (start <= Length(S)) and (S[start] = ' ') do
        start := start + 1;
    
    finish := Length(S);
    while (finish >= start) and (S[finish] = ' ') do
        finish := finish - 1;
    
    if start > finish then
        Trim := ''
    else
        Trim := Copy(S, start, finish - start + 1);
end;

function TrimLeft(const S: AnsiString): AnsiString;
var
    start: integer;
begin
    start := 1;
    while (start <= Length(S)) and (S[start] = ' ') do
        start := start + 1;
    TrimLeft := Copy(S, start, Length(S) - start + 1);
end;

function TrimRight(const S: AnsiString): AnsiString;
var
    finish: integer;
begin
    finish := Length(S);
    while (finish >= 1) and (S[finish] = ' ') do
        finish := finish - 1;
    TrimRight := Copy(S, 1, finish);
end;

function AnsiUpperCase(const S: AnsiString): AnsiString;
begin
    AnsiUpperCase := UpperCase(S);
end;

function AnsiLowerCase(const S: AnsiString): AnsiString;
begin
    AnsiLowerCase := LowerCase(S);
end;

function CompareText(const S1, S2: AnsiString): Integer;
var
    A, B: AnsiString;
    i, minLen: Integer;
begin
    A := LowerCase(S1);
    B := LowerCase(S2);
    if Length(A) < Length(B) then
        minLen := Length(A)
    else
        minLen := Length(B);

    for i := 1 to minLen do
    begin
        if A[i] <> B[i] then
        begin
            CompareText := Ord(A[i]) - Ord(B[i]);
            exit;
        end;
    end;

    CompareText := Length(A) - Length(B);
end;

function SameText(const S1, S2: AnsiString): Boolean;
begin
    SameText := CompareText(S1, S2) = 0;
end;

function StringReplace(const S, OldPattern, NewPattern: AnsiString): AnsiString;
var
    i: integer;
    result: AnsiString;
    temp: AnsiString;
begin
    result := '';
    i := 1;
    
    while i <= Length(S) do
    begin
        if Copy(S, i, Length(OldPattern)) = OldPattern then
        begin
            result := result + NewPattern;
            i := i + Length(OldPattern);
        end
        else
        begin
            result := result + S[i];
            i := i + 1;
        end;
    end;
    
    StringReplace := result;
end;

function StringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
var
    i: integer;
    result: AnsiString;
    ignoreCase: Boolean;
    replaceAll: Boolean;
    oldLen: integer;
    matched: Boolean;
begin
    result := '';
    i := 1;
    oldLen := Length(OldPattern);
    ignoreCase := rfIgnoreCase in Flags;
    replaceAll := rfReplaceAll in Flags;
    
    while i <= Length(S) do
    begin
        if ignoreCase then
            matched := SameText(Copy(S, i, oldLen), OldPattern)
        else
            matched := Copy(S, i, oldLen) = OldPattern;
            
        if matched then
        begin
            result := result + NewPattern;
            i := i + oldLen;
            if not replaceAll then
            begin
                { Copy rest of string and exit }
                result := result + Copy(S, i, Length(S) - i + 1);
                StringReplace := result;
                exit;
            end;
        end
        else
        begin
            result := result + S[i];
            i := i + 1;
        end;
    end;
    
    StringReplace := result;
end;

function Pos(Substr: AnsiString; S: AnsiString): integer;
var
    i, j: integer;
    found: boolean;
begin
    if Length(Substr) = 0 then
    begin
        Pos := 0;
        exit;
    end;
    
    for i := 1 to Length(S) - Length(Substr) + 1 do
    begin
        found := true;
        for j := 1 to Length(Substr) do
        begin
            if S[i + j - 1] <> Substr[j] then
            begin
                found := false;
                break;
            end;
        end;
        if found then
        begin
            Pos := i;
            exit;
        end;
    end;
    
    Pos := 0;
end;

function FormatDateTime(const FormatStr: string; DateTime: TDateTime): AnsiString;
begin
    FormatDateTime := kgpc_format_datetime(ToPChar(FormatStr), DateTime);
end;

function DateTimeToStr(DateTime: TDateTime): AnsiString;
begin
    DateTimeToStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', DateTime);
end;

function TimeToStr(DateTime: TDateTime): AnsiString;
begin
    TimeToStr := FormatDateTime('hh:nn:ss', DateTime);
end;

function UnixToDateTime(UnixTime: Int64): TDateTime;
begin
    { FPC: seconds since 1970-01-01 -> TDateTime days since 1899-12-30 }
    UnixToDateTime := (UnixTime / 86400.0) + 25569.0;
end;

function Format(const Fmt: string; const Args: array of const): string;
var
    ArgPointer: Pointer;
begin
    if Length(Args) > 0 then
        ArgPointer := @Args[Low(Args)]
    else
        ArgPointer := nil;
    Format := kgpc_format(Fmt, ArgPointer, Length(Args));
end;

function FloatToStr(Value: Real): AnsiString;
begin
    FloatToStr := Format('%.6f', [Value]);
end;

function TryStrToInt(const S: AnsiString; out Value: Longint): Boolean;
begin
    Result := kgpc_string_to_int(ToPChar(S), Value) <> 0;
end;

function TryStrToFloat(const S: AnsiString; out Value: Real): Boolean;
begin
    Result := kgpc_string_to_real(ToPChar(S), Value) <> 0;
end;

function StrToInt(const S: AnsiString): longint;
begin
    if not TryStrToInt(S, Result) then
        Result := 0;
end;

function StrToFloat(const S: AnsiString): real;
begin
    if not TryStrToFloat(S, Result) then
        Result := 0.0;
end;

function AnsiCompareStr(const S1, S2: AnsiString): Integer;
begin
    Result := kgpc_string_compare(ToPChar(S1), ToPChar(S2));
end;

function AnsiPos(const SubStr, S: AnsiString): Integer;
begin
    Result := kgpc_string_pos(ToPChar(SubStr), ToPChar(S));
end;

function ExtractFilePath(const FileName: AnsiString): AnsiString;
begin
    ExtractFilePath := kgpc_extract_file_path(ToPChar(FileName));
end;

function ExtractFileName(const FileName: AnsiString): AnsiString;
begin
    ExtractFileName := kgpc_extract_file_name(ToPChar(FileName));
end;

function ExtractFileExt(const FileName: AnsiString): AnsiString;
begin
    ExtractFileExt := kgpc_extract_file_ext(ToPChar(FileName));
end;

function ChangeFileExt(const FileName, Extension: AnsiString): AnsiString;
begin
    ChangeFileExt := kgpc_change_file_ext(ToPChar(FileName), ToPChar(Extension));
end;

function IncludeTrailingPathDelimiter(const Dir: AnsiString): AnsiString;
begin
  if (Dir = '') or (Copy(Dir, Length(Dir), 1) = PathDelim) or (Copy(Dir, Length(Dir), 1) = AltPathDelim) then
    IncludeTrailingPathDelimiter := Dir
  else
    IncludeTrailingPathDelimiter := Dir + PathDelim;
end;

function ExcludeTrailingPathDelimiter(const Dir: AnsiString): AnsiString;
begin
    ExcludeTrailingPathDelimiter := kgpc_exclude_trailing_path_delim(ToPChar(Dir));
end;

function FileExists(const FileName: AnsiString): Boolean;
begin
  Result := kgpc_file_exists(ToPChar(FileName)) <> 0;
end;

function DeleteFile(const FileName: AnsiString): Boolean;
begin
  Result := kgpc_delete_file(ToPChar(FileName)) <> 0;
end;

function DirectoryExists(const DirName: AnsiString): Boolean;
begin
  Result := kgpc_directory_exists(ToPChar(DirName)) <> 0;
end;

function RenameFile(const OldName, NewName: AnsiString): Boolean;
begin
  Result := kgpc_file_rename(ToPChar(OldName), ToPChar(NewName)) = 0;
end;

function GetCurrentDir: AnsiString;
begin
    GetCurrentDir := kgpc_get_current_dir();
end;

function SetCurrentDir(const Dir: AnsiString): Boolean;
begin
    SetCurrentDir := kgpc_set_current_dir(ToPChar(Dir)) = 0;
end;

function GetEnvironmentVariable(const Name: AnsiString): AnsiString;
begin
    GetEnvironmentVariable := kgpc_get_environment_variable(ToPChar(Name));
end;

function SetEnvironmentVariable(const Name, Value: AnsiString): Boolean;
begin
    SetEnvironmentVariable := kgpc_set_environment_variable(ToPChar(Name), ToPChar(Value)) = 0;
end;

function UnsetEnvironmentVariable(const Name: AnsiString): Boolean;
begin
    UnsetEnvironmentVariable := kgpc_unset_environment_variable(ToPChar(Name)) = 0;
end;

function GetProcessID: Longint;
begin
    GetProcessID := kgpc_get_process_id();
end;

function LoadLibrary(const Name: AnsiString): NativeUInt;
begin
    LoadLibrary := kgpc_load_library(ToPChar(Name));
end;

function GetProcedureAddress(LibHandle: NativeUInt; const ProcName: AnsiString): NativeUInt;
begin
    GetProcedureAddress := kgpc_get_proc_address(LibHandle, ToPChar(ProcName));
end;

function FreeLibrary(LibHandle: NativeUInt): Boolean;
begin
    FreeLibrary := kgpc_free_library(LibHandle) <> 0;
end;

procedure FreeAndNil(var Obj: Pointer);
var
    Temp: Pointer;
begin
    { Note: This is a simplified implementation.
      A full implementation would call the object's destructor. }
    Temp := Obj;
    if Temp <> nil then
    begin
        { Set to nil. In a complete implementation, this would
          call the object's destructor first. }
        Obj := nil;
    end;
end;

procedure SetString(out S: AnsiString; Buffer: PAnsiChar; Len: Integer);
var
    i: Integer;
    P: PAnsiChar;
    C: AnsiChar;
begin
    if (Buffer = nil) or (Len <= 0) then
        S := ''
    else
    begin
        SetLength(S, Len);
        P := Buffer;
        for i := 1 to Len do
        begin
            C := P^;
            S[i] := Chr(Ord(C));
            P := PAnsiChar(NativeUInt(P) + 1);
        end;
    end;
end;

constructor Exception.Create(const Msg: AnsiString);
begin
    FMessage := Msg;
end;

constructor Exception.CreateFmt(const Msg: AnsiString; const Args: array of const);
var
    ArgPointer: Pointer;
    formatted: AnsiString;
begin
    if Length(Args) > 0 then
        ArgPointer := @Args[Low(Args)]
    else
        ArgPointer := nil;
    formatted := kgpc_format(Msg, ArgPointer, Length(Args));
    FMessage := formatted;
end;

function FileDateToDateTime(FileDate: LongInt): TDateTime;
begin
    FileDateToDateTime := UnixToDateTime(FileDate);
end;

function StringToGUID(const S: AnsiString): TGUID;
var
    Hex: AnsiString;
    i: Integer;
    idx: Integer;
    ch: AnsiChar;
    value: Integer;
    function HexValue(C: AnsiChar): Integer;
    begin
        if (C >= '0') and (C <= '9') then
            HexValue := Ord(C) - Ord('0')
        else if (C >= 'A') and (C <= 'F') then
            HexValue := 10 + Ord(C) - Ord('A')
        else if (C >= 'a') and (C <= 'f') then
            HexValue := 10 + Ord(C) - Ord('a')
        else
            HexValue := -1;
    end;
    function ParseHex(StartIndex, Count: Integer): LongWord;
    var
        j: Integer;
        v: LongWord;
        digit: Integer;
    begin
        v := 0;
        for j := 0 to Count - 1 do
        begin
            digit := HexValue(Hex[StartIndex + j]);
            if digit < 0 then
            begin
                ParseHex := 0;
                exit;
            end;
            v := (v shl 4) or LongWord(digit);
        end;
        ParseHex := v;
    end;
begin
    Hex := '';
    for i := 1 to Length(S) do
    begin
        ch := S[i];
        if (ch = '{') or (ch = '}') or (ch = '-') then
            continue;
        if HexValue(ch) >= 0 then
            Hex := Hex + ch;
    end;

    if Length(Hex) <> 32 then
    begin
        StringToGUID.D1 := 0;
        StringToGUID.D2 := 0;
        StringToGUID.D3 := 0;
        for i := 0 to 7 do
            StringToGUID.D4[i] := 0;
        exit;
    end;

    StringToGUID.D1 := ParseHex(1, 8);
    StringToGUID.D2 := Word(ParseHex(9, 4));
    StringToGUID.D3 := Word(ParseHex(13, 4));
    idx := 17;
    for i := 0 to 7 do
    begin
        value := Integer(ParseHex(idx, 2));
        StringToGUID.D4[i] := Byte(value);
        idx := idx + 2;
    end;
end;


end.
