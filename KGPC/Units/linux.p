unit linux;
interface

uses baseunix;

type
  TStatx = record
    stx_mask: Cardinal;
    stx_blksize: Cardinal;
    stx_attributes: QWord;
    stx_nlink: Cardinal;
    stx_uid: Cardinal;
    stx_gid: Cardinal;
    stx_mode: Word;
    stx_ino: QWord;
    stx_size: QWord;
    stx_blocks: QWord;
    stx_attributes_mask: QWord;
    stx_atime: TTimeSpec;
    stx_btime: TTimeSpec;
    stx_ctime: TTimeSpec;
    stx_mtime: TTimeSpec;
    stx_rdev_major: Cardinal;
    stx_rdev_minor: Cardinal;
    stx_dev_major: Cardinal;
    stx_dev_minor: Cardinal;
  end;

const
  STATX_ALL = $FFF;
  STATX_MTIME = $40;
  STATX_MODE = $2;
  STATX_ATIME = $20;
  STATX_CTIME = $80;
  AT_SYMLINK_NOFOLLOW = $100;
  AT_EMPTY_PATH = $1000;
  CLOCK_MONOTONIC = 1;

function statx(dirfd: cint; path: PAnsiChar; flags: cint; mask: Cardinal; var buf: TStatx): cint; external;
function clock_gettime(clk_id: cint; tp: Pointer): cint; external;

implementation
end.
