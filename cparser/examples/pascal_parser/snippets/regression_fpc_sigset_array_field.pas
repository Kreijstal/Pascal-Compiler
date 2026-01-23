{ Regression test: Array type in record field resolution }
{ FPC RTL baseunix.pp has sigactionrec with sigset_t array field }
{ This test documents an issue with array type fields in records }
unit regression_fpc_sigset_array_field;

interface

type
  cuLong = QWord;
  
const
  wordsinsigset = 16;
  
type
  sigset_t = array[0..wordsinsigset-1] of cuLong;
  
  sigactionrec = record
    sa_handler: Pointer;
    sa_flags: cuLong;
    sa_restorer: Pointer;
    sa_mask: sigset_t;  { Array type field }
  end;

procedure TestFillChar;

implementation

procedure TestFillChar;
var
  sa: sigactionrec;
begin
  { This pattern from FPC's bunxovl.inc - sizeof on array field }
  FillChar(sa.sa_mask, sizeof(sa.sa_mask), #0);
end;

end.
