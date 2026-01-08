unit baseunix;
interface

type
  cint = Integer;
  clong = Int64;
  cuint = Cardinal;
  culong = QWord;
  size_t = QWord;
  ssize_t = Int64;
  off_t = Int64;
  TOff = off_t;
  mode_t = Cardinal;
  dev_t = QWord;
  ino_t = QWord;
  nlink_t = QWord;
  uid_t = Cardinal;
  gid_t = Cardinal;
  time_t = Int64;
  
  TTimeVal = record
    tv_sec: time_t;
    tv_usec: clong;
  end;
  
  TTimeSpec = record
    tv_sec: time_t;
    tv_nsec: clong;
  end;
  
  Stat = record
    st_dev: dev_t;
    st_ino: ino_t;
    st_nlink: nlink_t;
    st_mode: mode_t;
    st_uid: uid_t;
    st_gid: gid_t;
    st_rdev: dev_t;
    st_size: off_t;
    st_blksize: clong;
    st_blocks: clong;
    st_atime: time_t;
    st_mtime: time_t;
    st_ctime: time_t;
  end;
  TStat = Stat;
  PStat = ^Stat;
  
  TStatfs = record
    bsize: clong;
    blocks: QWord;
    bfree: QWord;
    bavail: QWord;
    files: QWord;
    ffree: QWord;
  end;
  
  sigset_t = QWord;
  
  sigactionrec = record
    sa_handler: Pointer;
    sa_flags: cint;
    sa_mask: sigset_t;
  end;
  TSigActionRec = sigactionrec;
  PSigActionRec = ^sigactionrec;

const
  O_RdOnly = 0;
  O_WrOnly = 1;
  O_RdWr = 2;
  O_Creat = $40;
  O_Trunc = $200;
  
  LOCK_SH = 1;
  LOCK_EX = 2;
  LOCK_NB = 4;
  LOCK_UN = 8;
  
  F_OK = 0;
  W_OK = 2;
  R_OK = 4;
  X_OK = 1;
  
  S_IWUSR = $80;
  
  SIG_DFL = 0;
  SIGINT = 2;
  SIGFPE = 8;
  SIGSEGV = 11;
  SIGILL = 4;
  SIGBUS = 7;
  SIGQUIT = 3;
  
  ESysEINTR = 4;
  ESysEAGAIN = 11;
  ESysEDEADLK = 35;
  ESysENOSYS = 38;
  ESysENOLCK = 37;
  
  AT_FDCWD = -100;

function fpOpen(path: PAnsiChar; flags: cint): cint; external;
function fpOpen(path: PAnsiChar; flags: cint; mode: mode_t): cint; external;
function fpClose(fd: cint): cint; external;
function fpRead(fd: cint; var buf; count: size_t): ssize_t; external;
function fpWrite(fd: cint; const buf; count: size_t): ssize_t; external;
function fplSeek(fd: cint; offset: off_t; whence: cint): off_t; external;
function fpStat(path: PAnsiChar; var buf: Stat): cint; external;
function fpFStat(fd: cint; var buf: Stat): cint; external;
function fpLStat(path: PAnsiChar; var buf: Stat): cint; external;
function fpStatFS(path: PAnsiChar; buf: Pointer): cint; external;
function fpAccess(path: PAnsiChar; mode: cint): cint; external;
function fpUnlink(path: PAnsiChar): cint; external;
function fpFtruncate(fd: cint; length: off_t): cint; external;
function fpflock(fd: cint; operation: cint): cint; external;
function fpfsync(fd: cint): cint; external;
function fpFork: cint; external;
function fpExecve(path: PAnsiChar; argv, envp: PPAnsiChar): cint; external;
procedure fpExit(status: cint); external;
function fpGetErrno: cint; external;
function fpsigaction(sig: cint; act, oact: PSigActionRec): cint; external;
function fpgettimeofday(tv: Pointer; tz: Pointer): cint; external;
function fpnanosleep(req, rem: Pointer): cint; external;
function fptime: time_t; external;

function fpS_ISDIR(m: mode_t): Boolean;
function fpS_ISREG(m: mode_t): Boolean;
function fpS_ISLNK(m: mode_t): Boolean;
function fpS_ISSOCK(m: mode_t): Boolean;
function fpS_ISBLK(m: mode_t): Boolean;
function fpS_ISCHR(m: mode_t): Boolean;
function fpS_ISFIFO(m: mode_t): Boolean;

function FpRename(oldname, newname: PAnsiChar): cint; external;

var
  envp: PPAnsiChar; external name 'environ';

implementation

function fpS_ISDIR(m: mode_t): Boolean;
begin
  Result := (m and $F000) = $4000;
end;

function fpS_ISREG(m: mode_t): Boolean;
begin
  Result := (m and $F000) = $8000;
end;

function fpS_ISLNK(m: mode_t): Boolean;
begin
  Result := (m and $F000) = $A000;
end;

function fpS_ISSOCK(m: mode_t): Boolean;
begin
  Result := (m and $F000) = $C000;
end;

function fpS_ISBLK(m: mode_t): Boolean;
begin
  Result := (m and $F000) = $6000;
end;

function fpS_ISCHR(m: mode_t): Boolean;
begin
  Result := (m and $F000) = $2000;
end;

function fpS_ISFIFO(m: mode_t): Boolean;
begin
  Result := (m and $F000) = $1000;
end;

end.
