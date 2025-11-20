unit SysUtils;

interface

uses
    ctypes;

type
    TDateTime = Int64;
    AnsiString = string;
    PChar = ^Char;
    NativeInt = cint64;
    NativeUInt = cuint64;
    Uint64 = cuint64;

    // Added for Unix support
    AnsiChar = cchar;
    PAnsiChar = pcchar;
    Word = cuint16;
    Byte = cuint8;

const
    PathDelim = '/';
    AltPathDelim = '\';

procedure Sleep(milliseconds: integer);
function GetTickCount64: longint;
function MillisecondsBetween(startTick, endTick: TDateTime): Int64;
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
function Pos(Substr: AnsiString; S: AnsiString): integer;
function FormatDateTime(const FormatStr: string; DateTime: TDateTime): AnsiString;
function Format(const Fmt: string; const Args: array of const): string;
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

{ Generic procedure to free an object and set its reference to nil }
procedure FreeAndNil(var Obj: Pointer);

implementation

function gpc_format(fmt: AnsiString; args: Pointer; count: NativeUInt): AnsiString; external;
function gpc_string_to_int(text: PChar; out value: Integer): Integer; external;
function gpc_string_to_real(text: PChar; out value: Double): Integer; external;
function gpc_file_exists(path: PChar): Integer; external;
function gpc_directory_exists(path: PChar): Integer; external;
procedure gpc_sleep_ms(milliseconds: integer); external;
function gpc_get_tick_count64: NativeUInt; external;
function gpc_now: Int64; external;
function gpc_format_datetime(format: PChar; datetime_ms: Int64): AnsiString; external;
function gpc_string_compare(lhs: PChar; rhs: PChar): Integer; external;
function gpc_string_pos(SubStr: PChar; S: PChar): Integer; external;
function gpc_extract_file_path(path: PChar): AnsiString; external;
function gpc_extract_file_name(path: PChar): AnsiString; external;
function gpc_extract_file_ext(path: PChar): AnsiString; external;
function gpc_change_file_ext(path: PChar; extension: PChar): AnsiString; external;
function gpc_exclude_trailing_path_delim(path: PChar): AnsiString; external;
function gpc_delete_file(path: PChar): Integer; external;
function gpc_file_rename(old_path: PChar; new_path: PChar): Integer; external;
function gpc_get_current_dir: AnsiString; external;
function gpc_set_current_dir(path: PChar): Integer; external;
function gpc_get_environment_variable(name: PChar): AnsiString; external;
function gpc_set_environment_variable(name: PChar; value: PChar): Integer; external;
function gpc_unset_environment_variable(name: PChar): Integer; external;
function gpc_get_process_id: NativeUInt; external;
function gpc_load_library(path: PChar): NativeUInt; external;
function gpc_get_proc_address(handle: NativeUInt; symbol: PChar): NativeUInt; external;
function gpc_free_library(handle: NativeUInt): Integer; external;

function ToPChar(const S: AnsiString): PChar;
begin
    if S = '' then
        Result := nil
    else
        Result := @S[1];
end;

procedure Sleep(milliseconds: integer);
begin
    gpc_sleep_ms(milliseconds);
end;

function GetTickCount64: longint;
begin
    GetTickCount64 := gpc_get_tick_count64();
end;

function MillisecondsBetween(startTick, endTick: TDateTime): Int64;
begin
    if endTick >= startTick then
        MillisecondsBetween := endTick - startTick
    else
        MillisecondsBetween := startTick - endTick;
end;

function Now: TDateTime;
begin
    Now := gpc_now();
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
var
    dt_value: Int64;
begin
    if DateTime < 0 then
        dt_value := 0
    else
        dt_value := DateTime;
    FormatDateTime := gpc_format_datetime(ToPChar(FormatStr), dt_value);
end;

function Format(const Fmt: string; const Args: array of const): string;
var
    ArgPointer: Pointer;
begin
    if Length(Args) > 0 then
        ArgPointer := @Args[Low(Args)]
    else
        ArgPointer := nil;
    Format := gpc_format(Fmt, ArgPointer, Length(Args));
end;

function FloatToStr(Value: Real): AnsiString;
begin
    FloatToStr := Format('%.6f', [Value]);
end;

function TryStrToInt(const S: AnsiString; out Value: Longint): Boolean;
begin
    Result := gpc_string_to_int(ToPChar(S), Value) <> 0;
end;

function TryStrToFloat(const S: AnsiString; out Value: Real): Boolean;
begin
    Result := gpc_string_to_real(ToPChar(S), Value) <> 0;
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
    Result := gpc_string_compare(ToPChar(S1), ToPChar(S2));
end;

function AnsiPos(const SubStr, S: AnsiString): Integer;
begin
    Result := gpc_string_pos(ToPChar(SubStr), ToPChar(S));
end;

function ExtractFilePath(const FileName: AnsiString): AnsiString;
begin
    ExtractFilePath := gpc_extract_file_path(ToPChar(FileName));
end;

function ExtractFileName(const FileName: AnsiString): AnsiString;
begin
    ExtractFileName := gpc_extract_file_name(ToPChar(FileName));
end;

function ExtractFileExt(const FileName: AnsiString): AnsiString;
begin
    ExtractFileExt := gpc_extract_file_ext(ToPChar(FileName));
end;

function ChangeFileExt(const FileName, Extension: AnsiString): AnsiString;
begin
    ChangeFileExt := gpc_change_file_ext(ToPChar(FileName), ToPChar(Extension));
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
    ExcludeTrailingPathDelimiter := gpc_exclude_trailing_path_delim(ToPChar(Dir));
end;

function FileExists(const FileName: AnsiString): Boolean;
begin
  Result := gpc_file_exists(ToPChar(FileName)) <> 0;
end;

function DeleteFile(const FileName: AnsiString): Boolean;
begin
  Result := gpc_delete_file(ToPChar(FileName)) <> 0;
end;

function DirectoryExists(const DirName: AnsiString): Boolean;
begin
  Result := gpc_directory_exists(ToPChar(DirName)) <> 0;
end;

function RenameFile(const OldName, NewName: AnsiString): Boolean;
begin
  Result := gpc_file_rename(ToPChar(OldName), ToPChar(NewName)) = 0;
end;

function GetCurrentDir: AnsiString;
begin
    GetCurrentDir := gpc_get_current_dir();
end;

function SetCurrentDir(const Dir: AnsiString): Boolean;
begin
    SetCurrentDir := gpc_set_current_dir(ToPChar(Dir)) = 0;
end;

function GetEnvironmentVariable(const Name: AnsiString): AnsiString;
begin
    GetEnvironmentVariable := gpc_get_environment_variable(ToPChar(Name));
end;

function SetEnvironmentVariable(const Name, Value: AnsiString): Boolean;
begin
    SetEnvironmentVariable := gpc_set_environment_variable(ToPChar(Name), ToPChar(Value)) = 0;
end;

function UnsetEnvironmentVariable(const Name: AnsiString): Boolean;
begin
    UnsetEnvironmentVariable := gpc_unset_environment_variable(ToPChar(Name)) = 0;
end;

function GetProcessID: Longint;
begin
    GetProcessID := gpc_get_process_id();
end;

function LoadLibrary(const Name: AnsiString): NativeUInt;
begin
    LoadLibrary := gpc_load_library(ToPChar(Name));
end;

function GetProcedureAddress(LibHandle: NativeUInt; const ProcName: AnsiString): NativeUInt;
begin
    GetProcedureAddress := gpc_get_proc_address(LibHandle, ToPChar(ProcName));
end;

function FreeLibrary(LibHandle: NativeUInt): Boolean;
begin
    FreeLibrary := gpc_free_library(LibHandle) <> 0;
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

end.
