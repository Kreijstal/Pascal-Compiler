{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Marco van de Voort
    member of the Free Pascal development team.

    System unit for Linux.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ These things are set in the makefile, }
{ But you can override them here.}
{ If you use an aout system, set the conditional AOUT}
{ $Define AOUT}

Unit System;

{*****************************************************************************}
                                    interface
{*****************************************************************************}

 
 
 

 
 
 
 

{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 Marco van de Voort
    member of the Free Pascal development team.

    Target dependent defines used when compileing the baseunix unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

 
             // Keep using readdir system call instead
                                // of userland getdents stuff.
              // Allow uname with "domain" entry.
                                // (which is a GNU extension)
 

 

 

 

 
 
 

 

 

 

 

 

 

 

 

 


 

 


{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2001 by the Free Pascal development team

    This file contains the OS independent declarations of the system unit
    for unix styled systems

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

 
 

{
    This file contains the OS independent declarations of the system unit

    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2005 by the Free Pascal development team

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                             Needed switches
****************************************************************************}



{$MODE objfpc}
{$MODESWITCH advancedrecords}

 

{ At least 3.0.0 is required }
 

{ Using inlining for small system functions/wrappers }
{$INLINE on}
 
   
 

{ don't use FPU registervariables on the i386 and i8086 }
 

{ the assembler helpers need this}
 

 


{ needed for insert,delete,readln }
 
{ stack checking always disabled
  for system unit. This is because
  the startup code might not
  have been called yet when we
  get a stack error, this will
  cause big crashes
}
 

 

{ for now, the presence of unicode strings is just an approximation,
  USE_FILEREC_FULLNAME can be also enabled for other targets if
  they need file names longer than 255 chars }
 

{ allow for lightweight move of managed records }
 

{****************************************************************************
                         Global Types and Constants
****************************************************************************}

{ some values which are used in RTL for TSystemCodePage type }
const
  CP_ACP     = 0;     // default to ANSI code page
  CP_OEMCP   = 1;     // default to OEM (console) code page
  CP_UTF16   = 1200;  // utf-16
  CP_UTF16BE = 1201;  // unicodeFFFE
  CP_UTF7    = 65000; // utf-7
  CP_UTF8    = 65001; // utf-8
  CP_ASCII   = 20127; // us-ascii
  CP_NONE    = $FFFF; // rawbytestring encoding

Type
  { The compiler has all integer types defined internally. Here
    we define only aliases }
  DWord    = LongWord;
  Cardinal = LongWord;
  Integer  = SmallInt;
  UInt64   = QWord;

  { moved here from psystem.pas
    Delphi allows chose of overloaded procedure depending
    on Real <-> Double, so use type here, see also tw7425.pp (FK) }
 
  Real = type Double;
{ Include generic version of float64 record }
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2007 by Several contributors

    Generic mathematical routines (on type real)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{*************************************************************************}
{  Credits                                                                }
{*************************************************************************}
{       Copyright Abandoned, 1987, Fred Fish                              }
{                                                                         }
{       This previously copyrighted work has been placed into the         }
{       public domain by the author (Fred Fish) and may be freely used    }
{       for any purpose, private or commercial.  I would appreciate       }
{       it, as a courtesy, if this notice is left in all copies and       }
{       derivative works.  Thank you, and enjoy...                        }
{                                                                         }
{       The author makes no warranty of any kind with respect to this     }
{       product and explicitly disclaims any implied warranties of        }
{       merchantability or fitness for any particular purpose.            }
{-------------------------------------------------------------------------}
{       Copyright (c) 1992 Odent Jean Philippe                            }
{                                                                         }
{       The source can be modified as long as my name appears and some    }
{       notes explaining the modifications done are included in the file. }
{-------------------------------------------------------------------------}
{       Copyright (c) 1997 Carl Eric Codere                               }
{-------------------------------------------------------------------------}
{-------------------------------------------------------------------------
 Using functions from AMath/DAMath libraries, which are covered by the
 following license:

 (C) Copyright 2009-2013 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------}

 
type
{ also necessary for Int() on systems with 64bit floats (JM) }
{ case record required to get correct alignement for floating point type }
 
  float64 = record
    case byte of
      1: (dummy : double);
  
      2: (low,high: longword);
 
  end;
 
 
 

 
type
  float32 = longword;
 
 


 

 

 

 
 
  { win64 doesn't support the legacy fpu }
   
   
   
   
    ValReal = Extended;
   
 

   
   
 

 

 

 

 

 

 


 

 

 

 

 

 

 

 

 


{ By default enable a simple implementation of Random for 8/16 bit CPUs }
 


 
  FarPointer = Pointer;
 


 
  SizeInt = Int64;
  SizeUInt = QWord;
  PtrInt = Int64;
  PtrUInt = QWord;
  ValSInt = int64;
  ValUInt = qword;
  CodePointer = Pointer;
  CodePtrInt = PtrInt;
  CodePtrUInt = PtrUInt;
  TExitCode = Longint;
 

 

 

 
  ALUSInt = Int64;
  ALUUInt = QWord;
 

  { NativeInt and NativeUInt are Delphi compatibility types. Even though Delphi
    has IntPtr and UIntPtr, the Delphi documentation for NativeInt states that
    'The size of NativeInt is equivalent to the size of the pointer on the
    current platform'. Because of the misleading names, these types shouldn't be
    used in the FPC RTL. Note that on i8086 their size changes between 16-bit
    and 32-bit according to the memory model, so they're not really a 'native
    int' type there at all. }
  NativeInt  = Type PtrInt;
  NativeUInt = Type PtrUInt;

  Int8    = ShortInt;
  Int16   = SmallInt;
  Int32   = Longint;
  IntPtr  = PtrInt;
  UInt8   = Byte;
  UInt16  = Word;
  UInt32  = Cardinal;
  UIntPtr = PtrUInt;

 

{ Zero - terminated strings }

 
  // Compiler defines Char, we make AnsiChar an alias
  AnsiChar = char;
 

  PChar               = ^Char;
  PPChar              = ^PChar;
  PPPChar             = ^PPChar;

  TAnsiChar           = AnsiChar;
  PAnsiChar           = ^AnsiChar;
  PPAnsiChar          = ^PAnsiChar;
  PPPAnsiChar         = ^PPAnsiChar;

  UTF8Char = AnsiChar;
  PUTF8Char = PAnsiChar;

  UCS4Char            = 0..$10ffff;
  PUCS4Char           = ^UCS4Char;
 
  TUCS4CharArray      = array[0..$effffff] of UCS4Char;
 
  PUCS4CharArray      = ^TUCS4CharArray;
 
  UCS4String          = array of UCS4Char;
 

  UTF8String          = type AnsiString(CP_UTF8);
  PUTF8String         = ^UTF8String;
  RawByteString       = type AnsiString(CP_NONE);

  HRESULT             = type Longint;
 
  TDateTime           = type Double;
  TDate               = type TDateTime;
  TTime               = type TDateTime;
 
  TError               = type Longint;

 
  PSingle             = ^Single;
  PDouble             = ^Double;
  PExtended           = ^Extended;

  PPDouble            = ^PDouble;
 
  PCurrency           = ^Currency;
 
  PComp               = ^Comp;
 

  PSmallInt           = ^Smallint;
  PShortInt           = ^Shortint;
  PInteger            = ^Integer;
  PByte               = ^Byte;
  PWord               = ^word;
  PDWord              = ^DWord;
  PLongWord           = ^LongWord;
  PLongint            = ^Longint;
  PCardinal           = ^Cardinal;
  PQWord              = ^QWord;
  PInt64              = ^Int64;
  PUInt64             = ^UInt64;
  PPtrInt             = ^PtrInt;
  PPtrUInt            = ^PtrUInt;
  PSizeInt            = ^SizeInt;
  PSizeUInt           = ^SizeUInt;

  PPByte              = ^PByte;
  PPLongint           = ^PLongint;

  PPointer            = ^Pointer;
  PPPointer           = ^PPointer;

  PCodePointer        = ^CodePointer;
  PPCodePointer       = ^PCodePointer;

  PBoolean            = ^Boolean;

  PBoolean8           = ^Boolean8;
  PBoolean16          = ^Boolean16;
  PBoolean32          = ^Boolean32;
  PBoolean64          = ^Boolean64;

  PByteBool           = ^ByteBool;
  PWordBool           = ^WordBool;
  PLongBool           = ^LongBool;
  PQWordBool          = ^QWordBool;

  PNativeInt 	      = ^NativeInt;
  PNativeUInt	      = ^NativeUint;
  PInt8   	      = PShortInt;
  PInt16  	      = PSmallint;
  PInt32  	      = PLongint;
  PIntPtr 	      = PPtrInt;
  PUInt8  	      = PByte;
  PUInt16 	      = PWord;
  PUInt32 	      = PDWord;
  PUintPtr	      = PPtrUInt;

  PShortString        = ^ShortString;
  PAnsiString         = ^AnsiString;
  PRawByteString      = ^RawByteString;

 
  PDate               = ^TDateTime;
  PDateTime           = ^TDateTime;
 
  PError              = ^TError;

 
  PVariant            = ^Variant;
  POleVariant         = ^OleVariant;
 

  PWideChar           = ^WideChar;
  PPWideChar          = ^PWideChar;
  PPPWideChar         = ^PPWideChar;
  WChar               = Widechar;
  UCS2Char            = WideChar;
  PUCS2Char           = PWideChar;
  PWideString         = ^WideString;

  UnicodeChar         = WideChar;
  PUnicodeChar        = ^UnicodeChar;
  PUnicodeString      = ^UnicodeString;

  PMarshaledString    = ^PWideChar;
  PMarshaledAString   = ^PAnsiChar;

  MarshaledString     = PWideChar;
  MarshaledAString    = PAnsiChar;

  TSystemCodePage     = Word;

  TFileTextRecChar    =  UnicodeChar ;
  PFileTextRecChar    = ^TFileTextRecChar;

  TTextLineBreakStyle = (tlbsLF,tlbsCRLF,tlbsCR);

{ opaque data type and related opaque pointer }
  TOpaqueData = record end;
  POpaqueData = ^TOpaqueData;
  OpaquePointer = type POpaqueData;

{ procedure type }
  TProcedure  = Procedure;

{ platform-dependent types }
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Platform specific information}
type
  { fd are int in C also for 64bit targets (x86_64) }
  THandle = Longint;
  
  { pthread_t is defined as an "unsigned long" }
  TThreadID = PtrUInt;

  TOSTimestamp = Int64;

  PRTLCriticalSection = ^TRTLCriticalSection;
 
 
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Peter Vreman
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ definition of pthread_mutex_t, because needed in both ptypes.inc and }
{ in sysosh.inc                                                        }

{ use a macro rather than a constant, so this name doesn't get exported
  from the system unit interface; macro's have to be on at this point
  because they're use to propagate the MUTEXTYPENAME here }

 

 
   
 

  TRTLCriticalSection = record
    case byte of
      0 : (
            __size : array[0..40-1] of AnsiChar;
          );
      1 : (
             __align : sizeint;
          );
      2 : (
             pad1, pad2, pad3  , pad4 : longint;
             { this field is guaranteed to stay at this offset by glibc for
               binary compatibility with static initialisers }
             __m_kind: longint;
          );
  end;

 
 
 
 

 
 

   
  TEntryInformationOS = record
    argc: longint;
    argv: PPAnsiChar;
    envp: PPAnsiChar;
    stkptr: pointer;
    stklen: sizeuint;
    haltproc: procedure(e:longint);cdecl;
  end;




{ platform-dependent defines }
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by Free Pascal development team

    This file contains platform-specific defines that are used in
    multiple RTL units.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ the single byte OS APIs always use UTF-8 }
{ define FPCRTL_FILESYSTEM_UTF8}

{ The OS supports a single byte file system operations API that we use }
 

{ The OS supports a two byte file system operations API that we use }
{ define FPCRTL_FILESYSTEM_TWO_BYTE_API}


{*****************************************************************************
                   TextRec/FileRec exported to allow compiler to take size
*****************************************************************************}

 
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    FileRec record definition


    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This file contains the definition of the filerec record.
  It is put separately, so it is available outside the system
  unit without sacrificing TP compatibility.
}

const
 
  filerecnamelength = 255;
 
type
 { using packed makes the compiler to generate ugly code on some CPUs, further
    using packed causes the compiler to handle arrays of text wrongly, see  see tw0754 e.g. on arm  }
  FileRec = Record
    Handle    : THandle;
 
    Mode      : longint;
 
    RecSize   : SizeInt;
    _private  : array[1..3 * SizeOf(SizeInt) + SizeOf(pointer) + 4 * SizeOf(codepointer)] of byte;
    UserData  : array[1..32] of byte;
    name      : array[0..filerecnamelength] of TFileTextRecChar;
 
  End;


 

{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Textrec record definition

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This file contains the definition of the textrec record.
  It is put separately, so it is available outside the system
  unit without sacrificing TP compatibility.
}

const
 
  TextRecNameLength = 256;
  TextRecBufSize    = 256;
 
type
  TLineEndStr = string [3];
  TextBuf = array[0..TextRecBufSize-1] of ansichar;
  TTextBuf = TextBuf;

  { using packed makes the compiler to generate ugly code on some CPUs, further
    using packed causes the compiler to handle arrays of text wrongly, see  see tw0754 e.g. on arm  }
  TextRec = Record
    Handle    : THandle;
 
    Mode      : longint;
 
    bufsize   : SizeInt;
    _private  : SizeInt;
    bufpos,
    bufend    : SizeInt;
    bufptr    : ^textbuf;
    openfunc,
    inoutfunc,
    flushfunc,
    closefunc : codepointer;
    UserData  : array[1..32] of byte;
    name      : array[0..textrecnamelength-1] of TFileTextRecChar;
    LineEnd   : TLineEndStr;
    buffer    : textbuf;
    CodePage  : TSystemCodePage;
 
  End;




type
  { Needed for fpc_get_output }
  PText               = ^Text;

  TEntryInformation = record
    InitFinalTable : Pointer;
    ThreadvarTablesTable : Pointer;
    ResourceStringTables : Pointer;
    ResStrInitTables : Pointer;
    ResLocation : Pointer;
    PascalMain : Procedure;
    valgrind_used : boolean;
     
    OS : TEntryInformationOS;
     
  end;


const
{ Maximum value of the biggest signed and unsigned integer type available}
  MaxSIntValue = High(ValSInt);
  MaxUIntValue = High(ValUInt);

{ max. values for longint and int}
  MaxLongint  = $7fffffff;
  MaxSmallint = 32767;

  MaxInt   = maxsmallint;

type
 
  IntegerArray  = array[0..$effffff] of Integer;
 
  PIntegerArray = ^IntegerArray;
 
  PointerArray = array [0..512*1024*1024-2] of Pointer;
 
  PPointerArray = ^PointerArray;

  TBoundArray = array of SizeInt;

 
  TPCharArray = packed array[0..(MaxLongint div SizeOf(PAnsiChar))-1] of PAnsiChar;
 
  PPCharArray = ^TPCharArray;

(* CtrlBreak set to true signalizes Ctrl-Break signal, otherwise Ctrl-C. *)
(* Return value of true means that the signal has been processed, false  *)
(* means that default handling should be used. *)
  TCtrlBreakHandler = function (CtrlBreak: boolean): boolean;

  Int128Rec = packed record
    case integer of
 
      0 : (Lo,Hi : QWord);
 
      1 : (DWords : Array[0..3] of DWord);
      2 : (Words : Array[0..7] of Word);
      3 : (Bytes : Array[0..15] of Byte);
  end;

{ Numbers for routines that have compiler magic }
{
    This file is part of the Free Pascal run time library and compiler.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Internal Function/Constant Evaluator numbers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
{ Internal functions }
   fpc_in_lo_word           = 1;
   fpc_in_hi_word           = 2;
   fpc_in_lo_long           = 3;
   fpc_in_hi_long           = 4;
   fpc_in_ord_x             = 5;
   fpc_in_length_string     = 6;
   fpc_in_chr_byte          = 7;
   fpc_in_write_x           = 14;
   fpc_in_writeln_x         = 15;
   fpc_in_read_x            = 16;
   fpc_in_readln_x          = 17;
   fpc_in_concat_x          = 18;
   fpc_in_assigned_x        = 19;
   fpc_in_str_x_string      = 20;
   fpc_in_ofs_x             = 21;
   fpc_in_sizeof_x          = 22;
   fpc_in_typeof_x          = 23;
   fpc_in_val_x             = 24;
   fpc_in_reset_x           = 25;
   fpc_in_rewrite_x         = 26;
   fpc_in_low_x             = 27;
   fpc_in_high_x            = 28;
   fpc_in_seg_x             = 29;
   fpc_in_pred_x            = 30;
   fpc_in_succ_x            = 31;
   fpc_in_reset_typedfile   = 32;
   fpc_in_rewrite_typedfile = 33;
   fpc_in_settextbuf_file_x = 34;
   fpc_in_inc_x             = 35;
   fpc_in_dec_x             = 36;
   fpc_in_include_x_y       = 37;
   fpc_in_exclude_x_y       = 38;
   fpc_in_break             = 39;
   fpc_in_continue          = 40;
   fpc_in_assert_x_y        = 41;
   fpc_in_addr_x            = 42;
   fpc_in_typeinfo_x        = 43;
   fpc_in_setlength_x       = 44;
   fpc_in_finalize_x        = 45;
   fpc_in_new_x             = 46;
   fpc_in_dispose_x         = 47;
   fpc_in_exit              = 48;
   fpc_in_copy_x            = 49;
   fpc_in_initialize_x      = 50;
   fpc_in_leave             = 51; {macpas}
   fpc_in_cycle             = 52; {macpas}
   fpc_in_slice             = 53;
   fpc_in_unaligned_x       = 54;
   fpc_in_get_frame         = 56;
   fpc_in_get_caller_addr   = 57;
   fpc_in_get_caller_frame  = 58;
   fpc_in_pack_x_y_z        = 59;
   fpc_in_unpack_x_y_z      = 60;
   fpc_in_bitsizeof_x       = 61;
   fpc_in_writestr_x        = 62;
   fpc_in_readstr_x         = 63;
   fpc_in_abs_long          = 64;
   fpc_in_ror_x             = 65;
   fpc_in_ror_x_x           = 66;
   fpc_in_rol_x             = 67;
   fpc_in_rol_x_x           = 68;
   fpc_objc_selector_x      = 69;
   fpc_objc_protocol_x      = 70;
   fpc_objc_encode_x        = 71;
   fpc_in_sar_x_y           = 72;
   fpc_in_sar_x             = 73;
   fpc_in_bsf_x             = 74;
   fpc_in_bsr_x             = 75;
   fpc_in_default_x         = 76;
   fpc_in_box_x             = 77; { managed platforms: wrap in class instance }
   fpc_in_unbox_x_y         = 78; { manage platforms: extract from class instance }
   fpc_in_popcnt_x          = 79;
   fpc_in_aligned_x         = 80;
   fpc_in_setstring_x_y_z   = 81;
   fpc_in_insert_x_y_z      = 82;
   fpc_in_delete_x_y_z      = 83;
   fpc_in_reset_typedfile_name   = 84;
   fpc_in_rewrite_typedfile_name = 85;
   fpc_in_and_assign_x_y    = 86;
   fpc_in_or_assign_x_y     = 87;
   fpc_in_xor_assign_x_y    = 88;
   fpc_in_sar_assign_x_y    = 89;
   fpc_in_shl_assign_x_y    = 90;
   fpc_in_shr_assign_x_y    = 91;
   fpc_in_rol_assign_x_y    = 92;
   fpc_in_ror_assign_x_y    = 93;
   fpc_in_neg_assign_x      = 94;
   fpc_in_not_assign_x      = 95;
   fpc_in_faraddr_x         = 97;
   fpc_in_volatile_x        = 98;

{ Internal constant functions }
   fpc_in_const_sqr        = 100;
   fpc_in_const_abs        = 101;
   fpc_in_const_odd        = 102;
   fpc_in_const_ptr        = 103;
   fpc_in_const_swap_word  = 104;
   fpc_in_const_swap_long  = 105;
   fpc_in_lo_qword         = 106;
   fpc_in_hi_qword         = 107;
   fpc_in_const_swap_qword = 108;
   fpc_in_prefetch_var     = 109;
   fpc_in_const_eh_return_data_regno = 110;

{ FPU functions }
   fpc_in_trunc_real       = 120;
   fpc_in_round_real       = 121;
   fpc_in_frac_real        = 122;
   fpc_in_int_real         = 123;
   fpc_in_exp_real         = 124;
   fpc_in_cos_real         = 125;
   fpc_in_pi_real          = 126;
   fpc_in_abs_real         = 127;
   fpc_in_sqr_real         = 128;
   fpc_in_sqrt_real        = 129;
   fpc_in_arctan_real      = 130;
   fpc_in_ln_real          = 131;
   fpc_in_sin_real         = 132;
   fpc_in_fma_single       = 133;
   fpc_in_fma_double       = 134;
   fpc_in_fma_extended     = 135;
   fpc_in_fma_float128     = 136;

{ MMX functions }
{ these contants are used by the mmx unit }

   { MMX }
   fpc_in_mmx_pcmpeqb      = 200;
   fpc_in_mmx_pcmpeqw      = 201;
   fpc_in_mmx_pcmpeqd      = 202;
   fpc_in_mmx_pcmpgtb      = 203;
   fpc_in_mmx_pcmpgtw      = 204;
   fpc_in_mmx_pcmpgtd      = 205;

   { 3DNow }

   { SSE }

   fpc_in_cpu_first        = 10000;
   fpc_in_x86_mm_first     = 11000;
   


{ CPU specific stuff }
{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by the Free Pascal development team.

    CPU specific system unit header file

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  {

    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  fpc_in_x86_inportb = fpc_in_cpu_first;
  fpc_in_x86_inportw = fpc_in_cpu_first+1;
  fpc_in_x86_inportl = fpc_in_cpu_first+2;
  fpc_in_x86_outportb = fpc_in_cpu_first+3;
  fpc_in_x86_outportw = fpc_in_cpu_first+4;
  fpc_in_x86_outportl = fpc_in_cpu_first+5;
  fpc_in_x86_cli      = fpc_in_cpu_first+6;
  fpc_in_x86_sti      = fpc_in_cpu_first+7;
  fpc_in_x86_get_cs   = fpc_in_cpu_first+8;
  fpc_in_x86_get_ss   = fpc_in_cpu_first+9;
  fpc_in_x86_get_ds   = fpc_in_cpu_first+10;
  fpc_in_x86_get_es   = fpc_in_cpu_first+11;
  fpc_in_x86_get_fs   = fpc_in_cpu_first+12;
  fpc_in_x86_get_gs   = fpc_in_cpu_first+13;
  fpc_in_x86_pause    = fpc_in_cpu_first+14;
  
   { include automatically generated numbers }
    


  has_avx_support : boolean = false;
  has_avx2_support : boolean = false;

function fpc_x86_inportb(port : word) : byte;[internproc:fpc_in_x86_inportb];
function fpc_x86_inportw(port : word) : word;[internproc:fpc_in_x86_inportw];
function fpc_x86_inportl(port : word) : longint;[internproc:fpc_in_x86_inportl];
procedure fpc_x86_outportb(port : word;data : byte);[internproc:fpc_in_x86_outportb];
procedure fpc_x86_outportw(port : word;data : word);[internproc:fpc_in_x86_outportw];
procedure fpc_x86_outportl(port : word;data : longint);[internproc:fpc_in_x86_outportl];
procedure fpc_x86_cli;[internproc:fpc_in_x86_cli];
procedure fpc_x86_sti;[internproc:fpc_in_x86_sti];
function fpc_x86_get_cs:longint;[internproc:fpc_in_x86_get_cs];
function fpc_x86_get_ss:longint;[internproc:fpc_in_x86_get_ss];
function fpc_x86_get_ds:longint;[internproc:fpc_in_x86_get_ds];
function fpc_x86_get_es:longint;[internproc:fpc_in_x86_get_es];
function fpc_x86_get_fs:longint;[internproc:fpc_in_x86_get_fs];
function fpc_x86_get_gs:longint;[internproc:fpc_in_x86_get_gs];
procedure fpc_x86_pause;[internproc:fpc_in_x86_pause];

{ include automatically generated procs }
 

type
  TNativeFPUControlWord = record
    cw8087: word;
    MXCSR: dword;
  end;


const
{ max level in dumping on error }
  Max_Frame_Dump : Word = 8;

{ Exit Procedure handling consts and types  }
  ExitProc : codepointer = nil;
  ErrorAddr: codepointer = nil;
  ErrorCode: Word    = 0;

{ file input modes }
  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;
  fmAppend = $D7B4;
  FileMode : byte = 2;
(* Value should be changed during system initialization as appropriate. *)

implementation end.
