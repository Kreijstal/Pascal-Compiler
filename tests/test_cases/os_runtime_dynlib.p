program OsRuntimeDynlib;

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
uses SysUtils, DynLibs;
{$endif}

const
{$ifdef MSWINDOWS}
    TestLib = 'kernel32.dll';
{$endif}

function BoolStr(Value: boolean): string;
begin
    if Value then
        BoolStr := 'TRUE'
    else
        BoolStr := 'FALSE';
end;

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
    Handle: TLibHandle;
    ProcPtr: Pointer;
    DynLibOK: boolean;
begin
{$ifdef MSWINDOWS}
    Handle := LoadLibrary(TestLib);
{$else}
    {$if defined(FPC) and not defined(KGPC_COMPILER)}
    Handle := TLibHandle(LoadUnixLibrary());
{$else}
    Handle := TLibHandle(LoadUnixLibrary());
{$endif}
{$endif}
    DynLibOK := False;
    if Handle <> 0 then
    begin
{$ifdef MSWINDOWS}
        ProcPtr := GetProcedureAddress(Handle, 'GetTickCount');
{$else}
        ProcPtr := GetProcedureAddress(Handle, 'strlen');
{$endif}
        DynLibOK := (ProcPtr <> nil) and FreeLibrary(Handle);
    end;
    WriteLn('DynLib=', BoolStr(DynLibOK));
end.
