{$mode objfpc}
{$H+}
{$macro on}
{$modeswitch typehelpers}
program tdd_sysutils_syshelpo_macro_helpers;

uses
  SysUtils;

resourcestring
  SInvalidInteger = 'Invalid integer';

type
  nibble = 0..15;
  TByteBitIndex = 0..7;
  TWordBitIndex = 0..15;
  TCardinalBitIndex = 0..31;
  TQwordBitIndex = 0..63;
  TQwordNibbleIndex = 0..15;
  TQwordByteIndex = 0..7;
  TQwordWordIndex = 0..3;
  TQwordDwordIndex = 0..1;

  TByteOverlay = bitpacked record case integer of
    0: (AsBit: bitpacked array[TByteBitIndex] of boolean);
    1: (AsNibble: bitpacked array[0..1] of nibble);
    2: (AsByte: byte);
  end;

  TWordOverlay = bitpacked record case integer of
    0: (AsBit: bitpacked array[TWordBitIndex] of boolean);
    1: (AsNibble: bitpacked array[0..3] of nibble);
    2: (AsByte: array[0..1] of byte);
    3: (AsWord: word);
    4: (AsByteOverlay: array[0..1] of TByteOverlay);
  end;

  TDwordOverlay = bitpacked record case integer of
    0: (AsBit: bitpacked array[TCardinalBitIndex] of boolean);
    1: (AsNibble: bitpacked array[0..7] of nibble);
    2: (AsByte: array[0..3] of byte);
    3: (AsWord: array[0..1] of word);
    4: (AsDword: dword);
    5: (AsByteOverlay: array[0..3] of TByteOverlay);
    6: (AsWordOverlay: array[0..1] of TWordOverlay);
  end;

  TQwordOverlay = bitpacked record case integer of
    0: (AsBit: bitpacked array[TQwordBitIndex] of boolean);
    1: (AsNibble: bitpacked array[TQwordNibbleIndex] of nibble);
    2: (AsByte: array[TQwordByteIndex] of byte);
    3: (AsWord: array[TQwordWordIndex] of word);
    4: (AsDword: array[TQwordDwordIndex] of dword);
    5: (AsQword: qword);
    6: (AsByteOverlay: array[TQwordByteIndex] of TByteOverlay);
    7: (AsWordOverlay: array[TQwordWordIndex] of TWordOverlay);
    8: (AsDwordOverlay: array[TQwordDwordIndex] of TDwordOverlay);
  end;

  TQWordHelper = type helper for QWord
  const
    MaxValue = High(QWord);
    MinValue = Low(QWord);
    MaxBit = High(TQwordBitIndex);
    MinBit = Low(TQwordBitIndex);
    MaxNibble = High(TQwordNibbleIndex);
    MinNibble = Low(TQwordNibbleIndex);
    MaxByte = High(TQwordByteIndex);
    MinByte = Low(TQwordByteIndex);
    MaxWord = High(TQwordWordIndex);
    MinWord = Low(TQwordWordIndex);
    MaxDword = High(TQwordDwordIndex);
    MinDword = Low(TQwordDwordIndex);
  public
    class function Parse(const AString: string): Qword; inline; static;
    class function Size: Integer; inline; static;
    class function ToString(const AValue: Qword): string; overload; inline; static;
    class function TryParse(const AString: string; out AValue: Qword): Boolean; inline; static;
  protected
    function GetBit(const aIndex: TQwordBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TQwordBitIndex; const aNewValue: boolean); inline;
    function GetNibble(const aIndex: TQwordNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TQwordNibbleIndex; const aNewValue: nibble); inline;
    function GetByte(const aIndex: TQwordByteIndex): byte;
    procedure PutByte(const aIndex: TQwordByteIndex; const aNewValue: byte);
    function GetWord(const aIndex: TQwordWordIndex): word;
    procedure PutWord(const aIndex: TQwordWordIndex; const aNewValue: word);
    function GetDword(const aIndex: TQwordDwordIndex): dword;
    procedure PutDword(const aIndex: TQwordDwordIndex; const aNewValue: dword);
  public
    function ToBoolean: Boolean; inline;
    function ToDouble: Double; inline;
    function ToExtended: Extended; inline;
    function ToBinString: string; inline;
    function ToHexString(const AMinDigits: Integer): string; overload; inline;
    function ToHexString: string; overload; inline;
    function ToSingle: Single; inline;
    function ToString: string; overload; inline;
    function SetBit(const Index: TQwordBitIndex): Qword; inline;
    function ClearBit(const Index: TQwordBitIndex): Qword; inline;
    function ToggleBit(const Index: TQwordBitIndex): Qword; inline;
    function TestBit(const Index: TQwordBitIndex): Boolean; inline;
    procedure Clear; inline;
    function HighestSetBitPos: int8; inline;
    function LowestSetBitPos: int8; inline;
    function SetBitsCount: byte; inline;
    property Bits[aIndex: TQwordBitIndex]: boolean read GetBit write PutBit;
    property Nibbles[aIndex: TQwordNibbleIndex]: nibble read GetNibble write PutNibble;
    property Bytes[aIndex: TQwordByteIndex]: byte read GetByte write PutByte;
    property Words[aIndex: TQwordWordIndex]: word read GetWord write PutWord;
    property DWords[aIndex: TQwordDwordIndex]: dword read GetDWord write PutDWord;
  end;

{$define TORDINALHELPER:=TQWordHelper}
{$define TORDINALTYPE:=QWord}
{$define TORDINALBITINDEX:=TQWordBitIndex}
{$define TORDINALNIBBLEINDEX:=TQWordNibbleIndex}
{$define TORDINALBYTEINDEX:=TQWordByteIndex}
{$define TORDINALWORDINDEX:=TQWordWordIndex}
{$define TORDINALDWORDINDEX:=TQWordDWordIndex}
{$define TORDINALOVERLAY:=TQWordOverlay}
{$define TORDINALTYPESIZE8}
{$i ../../FPCSource/rtl/objpas/sysutils/syshelpo.inc}
{$undef TORDINALTYPESIZE8}
{$undef TORDINALHELPER}
{$undef TORDINALTYPE}
{$undef TORDINALBITINDEX}
{$undef TORDINALNIBBLEINDEX}
{$undef TORDINALBYTEINDEX}
{$undef TORDINALWORDINDEX}
{$undef TORDINALDWORDINDEX}
{$undef TORDINALOVERLAY}

var
  Value: QWord;
  Bits: Byte;
  Hex: string;
  Byte2: Byte;
  Word1: Word;
  DWord0: DWord;
begin
  Value := $0123456789ABCDEF;
  Value := Value.SetBit(63);
  Value := Value.ClearBit(0);
  Value := Value.ToggleBit(4);
  Value.PutBit(9, True);
  Value.PutBit(12, False);
  Value.PutNibble(0, nibble((Value.GetNibble(0) + 3) and $F));
  Value.PutByte(2, $5A);
  Value.PutWord(1, $BEEF);
  Value.PutDWord(0, $DEADBEEF);

  Bits := Value.SetBitsCount;
  Hex := Value.ToHexString(16);
  Byte2 := Value.GetByte(2);
  Word1 := Value.GetWord(1);
  DWord0 := Value.GetDWord(0);

  Writeln('hex=', Hex);
  Writeln('bits=', Bits);
  Writeln('hsb=', Value.HighestSetBitPos);
  Writeln('lsb=', Value.LowestSetBitPos);
  Writeln('byte2=', IntToHex(Byte2, 2));
  Writeln('word1=', IntToHex(Word1, 4));
  Writeln('dword0=', IntToHex(DWord0, 8));
end.
