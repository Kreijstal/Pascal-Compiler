unit Unix;

interface

uses
    ctypes;

type
    RawByteString = string;

function GetHostName: string;
function GetDomainName: string;
function WaitProcess(Pid: cint): cint;
function W_EXITCODE(ReturnCode: integer; Signal: integer): integer;
function W_STOPCODE(Signal: integer): integer;
function WIFSTOPPED(Status: integer): boolean;

var
    tzdaylight: boolean;
    tzname: array[boolean] of PChar;

implementation

function gpc_unix_get_hostname_string: string; external;
function gpc_unix_get_domainname_string: string; external;
function gpc_unix_wait_process(pid: cint): cint; external;
function gpc_unix_w_exitcode(return_code: integer; signal_code: integer): integer; external;
function gpc_unix_w_stopcode(signal_code: integer): integer; external;
function gpc_unix_wifstopped(status: integer): cint; external;

function GetHostName: string;
begin
    GetHostName := gpc_unix_get_hostname_string;
end;

function GetDomainName: string;
begin
    GetDomainName := gpc_unix_get_domainname_string;
end;

function WaitProcess(Pid: cint): cint;
begin
    WaitProcess := gpc_unix_wait_process(Pid);
end;

function W_EXITCODE(ReturnCode: integer; Signal: integer): integer;
begin
    W_EXITCODE := gpc_unix_w_exitcode(ReturnCode, Signal);
end;

function W_STOPCODE(Signal: integer): integer;
begin
    W_STOPCODE := gpc_unix_w_stopcode(Signal);
end;

function WIFSTOPPED(Status: integer): boolean;
begin
    WIFSTOPPED := gpc_unix_wifstopped(Status) <> 0;
end;

end.
