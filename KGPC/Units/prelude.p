program prelude;

{ HARD COMPILER BUILTINS ONLY.
  This file is loaded in --no-stdlib mode BEFORE the FPC RTL system unit.
  It provides type aliases that the compiler C code references by name
  but does not register as C builtins.

  DO NOT add class declarations (TObject, TInterfacedObject) or complex
  record types (TextRec, FileRec, TVarRec) here. Those MUST come from
  the FPC RTL system unit. Adding incomplete stubs here shadows the
  proper FPC declarations and causes missing-field errors.

  Primitive types (Byte, Word, Cardinal, Int64, String, Char, Boolean,
  Pointer, etc.) are registered as C builtins in SemCheck.c and do not
  need to be declared here either. }

type
  { Size/pointer integer aliases - referenced by compiler C code }
  NativeInt = Int64;
  NativeUInt = QWord;
  SizeInt = NativeInt;
  SizeUInt = NativeUInt;
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;

  { Character and string aliases - referenced by compiler C code }
  AnsiChar = Char;
  PAnsiChar = ^AnsiChar;
  PPAnsiChar = ^PAnsiChar;
  PChar = ^Char;
  PPointer = ^Pointer;
  PWideChar = ^WideChar;

  { Pointer type aliases }
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
  TClass = class of TObject;
  TypedFile = file;
  TRTLCriticalSection = array[0..39] of Byte;
  TSystemCodePage = Word;

  THandle = LongInt;
  HRESULT = LongInt;
  CodePointer = Pointer;

  { String type aliases - compiler C code checks these names }
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
  PTextBuf = ^TextBuf;

  { TGUID - needed for interface support, compiler references fields by name }
  TGUID = record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
  end;

  TextFile = text;

  { Numeric type aliases }
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


type
  PShortString = ^ShortString;
  PDouble = ^Double;
  PSingle = ^Single;
  PExtended = ^Extended;
  PReal = ^Real;

  TTextLineBreakStyle = (tlbsLF, tlbsCRLF, tlbsCR);

  ByteBool = Boolean8;
  WordBool = Boolean16;
  LongBool = Boolean32;
  QWordBool = Boolean64;

const
  TextRecNameLength = 256;
  TextRecBufSize = 256;

  fmOutput = $D7B2;
  fmInOut = $D7B3;

begin
end.
