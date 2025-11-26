program OsRuntimeFeatures;

{$if defined(KGPC)}
  {$define KGPC_COMPILER}
{$endif}

{$if defined(FPC) and not defined(KGPC_COMPILER)}
{$mode objfpc}{$H+}
{$endif}

{$if defined(FPC) and not defined(KGPC_COMPILER)}
  {$ifdef MSWINDOWS}
  uses SysUtils, DynLibs, Windows;
  {$else}
  uses SysUtils, DynLibs, BaseUnix, ctypes;
  {$endif}
{$else}
uses SysUtils;
{$endif}

const
{$ifdef MSWINDOWS}
    TestLib = 'kernel32.dll';
{$endif}
    EnvVarName = 'KGPC_RTL_TEST_VAR';

function BoolStr(Value: boolean): string;
begin
    if Value then
        BoolStr := 'TRUE'
    else
        BoolStr := 'FALSE';
end;

{$if defined(FPC) and not defined(KGPC_COMPILER)}
{$ifdef MSWINDOWS}
function SetEnvironmentVariable(const Name, Value: string): Boolean;
begin
    Result := Windows.SetEnvironmentVariable(PChar(Name), PChar(Value));
end;

function UnsetEnvironmentVariable(const Name: string): Boolean;
begin
    Result := Windows.SetEnvironmentVariable(PChar(Name), nil);
end;

function GetProcessID: Longint;
begin
    GetProcessID := Windows.GetCurrentProcessId;
end;
{$else}
function c_setenv(name, value: PChar; overwrite: cint): cint; cdecl; external 'c' name 'setenv';
function c_unsetenv(name: PChar): cint; cdecl; external 'c' name 'unsetenv';
function SetEnvironmentVariable(const Name, Value: string): Boolean;
begin
    Result := c_setenv(PChar(AnsiString(Name)), PChar(AnsiString(Value)), 1) = 0;
end;

function UnsetEnvironmentVariable(const Name: string): Boolean;
begin
    Result := c_unsetenv(PChar(AnsiString(Name))) = 0;
end;

function GetProcessID: Longint;
begin
    GetProcessID := fpGetPid;
end;
{$endif}

{$endif}

function LoadUnixLibrary: NativeUInt;
begin
{$if defined(FPC) and not defined(KGPC_COMPILER)}
    Result := NativeUInt(LoadLibrary('libc.so.6'));
    if Result = 0 then
        Result := NativeUInt(LoadLibrary('libc.so'));
    if Result = 0 then
        Result := NativeUInt(LoadLibrary('libSystem.B.dylib'));
    if Result = 0 then
        Result := NativeUInt(LoadLibrary('libc.dylib'));
    if Result = 0 then
        Result := NativeUInt(LoadLibrary('cygwin1.dll'));
    if Result = 0 then
        Result := NativeUInt(LoadLibrary('msys-2.0.dll'));
{$else}
    Result := LoadLibrary('libc.so.6');
    if Result = 0 then
        Result := LoadLibrary('libc.so');
    if Result = 0 then
        Result := LoadLibrary('libSystem.B.dylib');
    if Result = 0 then
        Result := LoadLibrary('libc.dylib');
    if Result = 0 then
        Result := LoadLibrary('cygwin1.dll');
    if Result = 0 then
        Result := LoadLibrary('msys-2.0.dll');
{$endif}
end;

var
    StartDir: string;
    DirSetOK, DirRestoreOK: boolean;
begin
    StartDir := GetCurrentDir;
    DirSetOK := False;
    DirRestoreOK := False;
    if StartDir <> '' then
        DirSetOK := SetCurrentDir(StartDir);
    WriteLn('DirSet=', BoolStr(DirSetOK));

    if not SetEnvironmentVariable(EnvVarName, '42') then
    begin
    end;
    WriteLn('Env=', GetEnvironmentVariable(EnvVarName));
    if not UnsetEnvironmentVariable(EnvVarName) then
    begin
    end;
    WriteLn('EnvAfter=', GetEnvironmentVariable(EnvVarName));

    WriteLn('PIDValid=', BoolStr(GetProcessID > 0));

    if StartDir <> '' then
        DirRestoreOK := SetCurrentDir(StartDir);
    WriteLn('DirRestored=', BoolStr(DirRestoreOK));
end.
