unit Unixtype;
interface

uses baseunix;

type
  TUTimBuf = record
    actime: time_t;
    modtime: time_t;
  end;
  PUTimBuf = ^TUTimBuf;
  
  pdir = ^TDir;
  TDir = record
    fd: cint;
  end;
  
  pdirent = ^TDirent;
  TDirent = record
    d_ino: ino_t;
    d_off: off_t;
    d_reclen: Word;
    d_type: Byte;
    d_name: array[0..255] of AnsiChar;
  end;
  
  TTimespecArr = array[0..1] of TTimeSpec;

function fputime(path: PAnsiChar; times: PUTimBuf): cint; external;
function fpopendir(path: PAnsiChar): pdir; external;
function fpclosedir(var d: TDir): cint; external;
function fpreaddir(var d: TDir): pdirent; external;
function fpreadlink(path: PAnsiChar): String; external;
function utimensat(dirfd: cint; path: PAnsiChar; var times: TTimespecArr; flags: cint): cint; external;
function fpfutimens(fd: cint; var times: TTimespecArr): cint; external;

implementation
end.
