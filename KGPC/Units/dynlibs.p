unit DynLibs;

interface

type
    TLibHandle = NativeUInt;
    HModule = TLibHandle;

const
    NilHandle = TLibHandle(0);
    SharedSuffix = 'so';

function LoadLibrary(const Name: AnsiString): TLibHandle;
function SafeLoadLibrary(const Name: AnsiString): TLibHandle;
function UnloadLibrary(Lib: TLibHandle): Boolean;
function FreeLibrary(Lib: TLibHandle): Boolean;
function GetProcedureAddress(Lib: TLibHandle; const ProcName: AnsiString): Pointer;
function GetProcAddress(Lib: TLibHandle; const ProcName: AnsiString): Pointer;

implementation

function kgpc_load_library(path: PChar): NativeUInt; cdecl; external name 'kgpc_load_library';
function kgpc_free_library(handle: NativeUInt): LongInt; cdecl; external name 'kgpc_free_library';
function kgpc_get_proc_address(handle: NativeUInt; symbol: PChar): NativeUInt; cdecl; external name 'kgpc_get_proc_address';

function LoadLibrary(const Name: AnsiString): TLibHandle;
begin
    LoadLibrary := TLibHandle(kgpc_load_library(PChar(Name)));
end;

function SafeLoadLibrary(const Name: AnsiString): TLibHandle;
begin
    SafeLoadLibrary := LoadLibrary(Name);
end;

function UnloadLibrary(Lib: TLibHandle): Boolean;
begin
    UnloadLibrary := kgpc_free_library(NativeUInt(Lib)) = 0;
end;

function FreeLibrary(Lib: TLibHandle): Boolean;
begin
    FreeLibrary := UnloadLibrary(Lib);
end;

function GetProcedureAddress(Lib: TLibHandle; const ProcName: AnsiString): Pointer;
begin
    GetProcedureAddress := Pointer(kgpc_get_proc_address(NativeUInt(Lib), PChar(ProcName)));
end;

function GetProcAddress(Lib: TLibHandle; const ProcName: AnsiString): Pointer;
begin
    GetProcAddress := GetProcedureAddress(Lib, ProcName);
end;

end.
