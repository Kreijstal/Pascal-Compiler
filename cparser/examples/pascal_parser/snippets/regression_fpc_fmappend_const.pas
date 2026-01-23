{ Regression test: fmAppend constant }
{ FPC RTL baseunix.pp uses fmAppend in case statements }
{ This test documents a missing constant in KGPC stdlib }
unit regression_fpc_fmappend_const;

interface

const
  { File mode constants used by FPC RTL }
  fmClosed = $D7B0;
  fmInput = $D7B1;
  fmOutput = $D7B2;
  fmInOut = $D7B3;
  fmAppend = $D7B4;  { This constant is missing in KGPC }

type
  textrec = record
    mode: Word;
  end;

procedure CheckFileMode(var f: textrec);

implementation

procedure CheckFileMode(var f: textrec);
begin
  case f.mode of
    fmOutput, fmInOut, fmAppend:
      { Flush buffer }
      ;
  end;
end;

end.
