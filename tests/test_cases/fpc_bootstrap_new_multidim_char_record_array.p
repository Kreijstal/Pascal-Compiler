program fpc_bootstrap_new_multidim_char_record_array;

type
  TTokenIdxRec = record
    First: LongInt;
    Last: LongInt;
  end;
  TTokenIdx = array[1..18, 'A'..'Z'] of TTokenIdxRec;
  PTokenIdx = ^TTokenIdx;

var
  TokenIdx: PTokenIdx;

begin
  New(TokenIdx);
  WriteLn(SizeOf(TokenIdx^));
  Dispose(TokenIdx);
end.
