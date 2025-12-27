program fpc_bootstrap_longbool;

var
    lb: LongBool;
    wb: WordBool;
    bb: ByteBool;
    qwb: QWordBool;
    b16: Boolean16;
    b32: Boolean32;
    b64: Boolean64;

begin
    lb := True;
    wb := True;
    bb := True;
    qwb := True;
    b16 := True;
    b32 := True;
    b64 := True;
    
    WriteLn('LongBool size: ', SizeOf(lb));
    WriteLn('WordBool size: ', SizeOf(wb));
    WriteLn('ByteBool size: ', SizeOf(bb));
    WriteLn('QWordBool size: ', SizeOf(qwb));
    WriteLn('Boolean16 size: ', SizeOf(b16));
    WriteLn('Boolean32 size: ', SizeOf(b32));
    WriteLn('Boolean64 size: ', SizeOf(b64));
    
    if lb then WriteLn('LongBool is True');
    if wb then WriteLn('WordBool is True');
    if bb then WriteLn('ByteBool is True');
    if qwb then WriteLn('QWordBool is True');
end.
