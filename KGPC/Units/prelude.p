program prelude;

{ Minimal prelude for no-stdlib mode. Core primitive types still come from
  compiler intrinsics; this file provides the Pascal-level aliases/constants
  that were previously injected. }

type
  Byte = 0..255;
  ShortInt = -128..127;
  Word = 0..65535;
  SmallInt = -32768..32767;
  Cardinal = 0..4294967295;
  LongWord = Cardinal;
  DWord = Cardinal;

  QWord = Int64;
  UInt64 = QWord;

  Single = Real;
  Double = Real;
  Extended = Real;

  NativeInt = Int64;
  NativeUInt = QWord;
  SizeInt = NativeInt;
  SizeUInt = NativeUInt;
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;

  AnsiChar = Char;
  PAnsiChar = ^AnsiChar;
  PPAnsiChar = ^PAnsiChar;
  PChar = ^Char;
  PPointer = ^Pointer;
  PWideChar = ^WideChar;

  PByte = ^Byte;
  PWord = ^Word;
  PLongInt = ^LongInt;
  PLongWord = ^LongWord;
  PInteger = ^Integer;
  PCardinal = ^Cardinal;
  PShortInt = ^ShortInt;
  PSmallInt = ^SmallInt;
  PInt64 = ^Int64;
  PQWord = ^QWord;
  PBoolean = ^Boolean;

  PText = ^text;
  TObject = class;
  TClass = class of TObject;
  TypedFile = file;
  TRTLCriticalSection = array[0..39] of Byte;
  TSystemCodePage = Word;

  THandle = LongInt;
  HRESULT = LongInt;  { Windows COM result type }
  CodePointer = Pointer;

  AnsiString = String;
  UnicodeString = String;
  WideString = String;
  RawByteString = String;
  PAnsiString = ^AnsiString;
  PString = ^String;
  ShortString = array[0..255] of Char;
  PWideString = ^WideString;

  TLineEndStr = string[3];
  TextBuf = array[0..255] of AnsiChar;
  TTextBuf = TextBuf;

  TObject = class
  end;
  TInterfacedObject = class(TObject)
  end;

  TGUID = record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
  end;

  TextRec = record
    Handle: THandle;
    Mode: LongInt;
    BufSize: SizeInt;
    PrivateData: SizeInt;
    BufPos: SizeInt;
    BufEnd: SizeInt;
    BufPtr: ^TextBuf;
    OpenFunc: CodePointer;
    InOutFunc: CodePointer;
    FlushFunc: CodePointer;
    CloseFunc: CodePointer;
    UserData: array[1..32] of Byte;
    Name: array[0..255] of AnsiChar;
    LineEnd: TLineEndStr;
    Buffer: TextBuf;
    CodePage: TSystemCodePage;
  end;

  FileRec = record
    Handle: THandle;
    Mode: LongInt;
    RecSize: SizeInt;
    PrivateData: array[1..64] of Byte;
    UserData: array[1..32] of Byte;
    Name: array[0..255] of AnsiChar;
  end;

  Int8 = ShortInt;
  UInt8 = Byte;
  Int16 = SmallInt;
  UInt16 = Word;
  Int32 = LongInt;
  UInt32 = Cardinal;

  TDateTime = type Double;
  Comp = Int64;
  Currency = Int64;
  PCurrency = ^Currency;

  Variant = Pointer;
  PVariant = ^Variant;

const
  vtInteger = 0;
  vtBoolean = 1;
  vtChar = 2;
  vtExtended = 3;
  vtString = 4;
  vtPointer = 5;
  vtPChar = 6;
  vtObject = 7;
  vtClass = 8;
  vtWideChar = 9;
  vtPWideChar = 10;
  vtAnsiString = 11;
  vtCurrency = 12;
  vtVariant = 13;
  vtInterface = 14;
  vtWideString = 15;
  vtInt64 = 16;
  vtQWord = 17;
  vtUnicodeString = 18;

type
  PVarRec = ^TVarRec;
  TVarRec = record
    case VType: SizeInt of
      vtInteger: (VInteger: LongInt);
      vtBoolean: (VBoolean: Boolean);
      vtChar: (VChar: AnsiChar);
      vtWideChar: (VWideChar: WideChar);
      vtExtended: (VExtended: PExtended);
      vtString: (VString: PShortString);
      vtPointer: (VPointer: Pointer);
      vtPChar: (VPChar: PAnsiChar);
      vtObject: (VObject: TObject);
      vtClass: (VClass: TClass);
      vtPWideChar: (VPWideChar: PWideChar);
      vtAnsiString: (VAnsiString: Pointer);
      vtCurrency: (VCurrency: PCurrency);
      vtVariant: (VVariant: PVariant);
      vtInterface: (VInterface: Pointer);
      vtWideString: (VWideString: Pointer);
      vtInt64: (VInt64: PInt64);
      vtUnicodeString: (VUnicodeString: Pointer);
      vtQWord: (VQWord: PQWord);
  end;

  PShortString = ^ShortString;
  PDouble = ^Double;
  PSingle = ^Single;
  PExtended = ^Extended;
  PReal = ^Real;

  TTextLineBreakStyle = (tlbsLF, tlbsCRLF, tlbsCR);

  TSignalState = (ssNotHooked, ssHooked, ssOverridden);

  ByteBool = Boolean8;
  WordBool = Boolean16;
  LongBool = Boolean32;
  QWordBool = Boolean64;

const
  TextRecNameLength = 256;
  TextRecBufSize = 256;

  LineEnding = #10;
  sLineBreak = LineEnding;
  DirectorySeparator: AnsiChar = '/';
  DriveSeparator: AnsiChar = #0;
  PathSeparator: AnsiChar = ':';
  ExtensionSeparator: AnsiChar = '.';
  AllowDirectorySeparators: set of AnsiChar = ['\', '/'];
  AllowDriveSeparators: set of AnsiChar = [];
  MaxPathLen = 4096;

  DefaultSystemCodePage = 65001;
  DefaultFileSystemCodePage = 65001;

  fmClosed = $D7B0;
  fmInput = $D7B1;
  fmOutput = $D7B2;
  fmInOut = $D7B3;

  ARG_MAX = 131072;
  NAME_MAX = 255;
  PATH_MAX = 4095;
  SYS_NMLN = 65;
  SIG_MAXSIG = 128;
  PRIO_PROCESS = 0;
  PRIO_PGRP = 1;
  PRIO_USER = 2;
  UTSNAME_LENGTH = 65;

  RTL_SIGINT = 0;
  RTL_SIGFPE = 1;
  RTL_SIGSEGV = 2;
  RTL_SIGILL = 3;
  RTL_SIGBUS = 4;
  RTL_SIGQUIT = 5;
  RTL_SIGLAST = RTL_SIGQUIT;
  RTL_SIGDEFAULT = -1;

var
  IsLibrary: Boolean = False;

begin
end.
