unit Unix;

interface

uses
    ctypes;

const
    { Signal constants for Linux x86-64 }
    SIGHUP     = 1;
    SIGINT     = 2;
    SIGQUIT    = 3;
    SIGILL     = 4;
    SIGTRAP    = 5;
    SIGABRT    = 6;
    SIGBUS     = 7;
    SIGFPE     = 8;
    SIGKILL    = 9;
    SIGUSR1    = 10;
    SIGSEGV    = 11;
    SIGUSR2    = 12;
    SIGPIPE    = 13;
    SIGALRM    = 14;
    SIGTERM    = 15;
    SIGCHLD    = 17;
    SIGCONT    = 18;
    SIGSTOP    = 19;
    SIGTSTP    = 20;
    SIGTTIN    = 21;
    SIGTTOU    = 22;

    { Signal action flags }
    SA_NOCLDSTOP = 1;
    SA_NOCLDWAIT = 2;
    SA_SIGINFO   = 4;
    SA_RESTORER  = $04000000;
    SA_ONSTACK   = $08000000;
    SA_RESTART   = $10000000;
    SA_NODEFER   = $40000000;
    SA_RESETHAND = $80000000;

    SIG_BLOCK   = 0;
    SIG_UNBLOCK = 1;
    SIG_SETMASK = 2;

    SIG_DFL = 0;
    SIG_IGN = 1;
    SIG_ERR = -1;

type
    RawByteString = string;

    { Signal set - 128 bytes on Linux x86-64 }
    sigset_t = array[0..15] of culong;
    tsigset = sigset_t;
    psigset = ^tsigset;

    { Signal info structure (simplified) }
    psiginfo = ^tsiginfo;
    tsiginfo = record
        si_signo: cint;
        si_errno: cint;
        si_code: cint;
        _pad: array[0..28] of cint;
    end;

    { Signal context (opaque for now) }
    psigcontext = pointer;

    { Signal handler types }
    signalhandler_t = procedure(signal: cint); cdecl;
    sigactionhandler_t = procedure(signal: cint; info: psiginfo; context: psigcontext); cdecl;
    sigrestorerhandler_t = procedure; cdecl;

    tsignalhandler = signalhandler_t;
    tsigactionhandler = sigactionhandler_t;

    { Signal action record for Linux x86-64 }
    psigactionrec = ^sigactionrec;
    sigactionrec = record
        sa_handler: sigactionhandler_t;
        sa_flags: culong;
        sa_restorer: sigrestorerhandler_t;
        sa_mask: sigset_t;
    end;
    SigActionRec = sigactionrec;

function GetHostName: string;
function GetDomainName: string;
function WaitProcess(Pid: cint): cint;
function W_EXITCODE(ReturnCode: integer; Signal: integer): integer;
function W_STOPCODE(Signal: integer): integer;
function WIFSTOPPED(Status: integer): boolean;

{ Signal functions }
function fpsigaction(sig: cint; act: psigactionrec; oact: psigactionrec): cint;

var
    tzdaylight: boolean;
    tzname: array[boolean] of PChar;

implementation

function kgpc_unix_get_hostname_string: string; external;
function kgpc_unix_get_domainname_string: string; external;
function kgpc_unix_wait_process(pid: cint): cint; external;
function kgpc_unix_w_exitcode(return_code: integer; signal_code: integer): integer; external;
function kgpc_unix_w_stopcode(signal_code: integer): integer; external;
function kgpc_unix_wifstopped(status: integer): cint; external;

function GetHostName: string;
begin
    GetHostName := kgpc_unix_get_hostname_string;
end;

function GetDomainName: string;
begin
    GetDomainName := kgpc_unix_get_domainname_string;
end;

function WaitProcess(Pid: cint): cint;
begin
    WaitProcess := kgpc_unix_wait_process(Pid);
end;

function W_EXITCODE(ReturnCode: integer; Signal: integer): integer;
begin
    W_EXITCODE := kgpc_unix_w_exitcode(ReturnCode, Signal);
end;

function W_STOPCODE(Signal: integer): integer;
begin
    W_STOPCODE := kgpc_unix_w_stopcode(Signal);
end;

function WIFSTOPPED(Status: integer): boolean;
begin
    WIFSTOPPED := kgpc_unix_wifstopped(Status) <> 0;
end;

function fpsigaction(sig: cint; act: psigactionrec; oact: psigactionrec): cint;
begin
    { Stub implementation - returns 0 (success) for now }
    fpsigaction := 0;
end;

end.
