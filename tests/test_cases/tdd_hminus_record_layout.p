program tdd_hminus_record_layout;
{$mode objfpc}
{$H-}

{ Under {$H-}, 'string' in record fields should be ShortString (256 bytes),
  not AnsiString (pointer = 8 bytes).  Issue #571: wrong record layout
  caused field offsets to be incorrect, leading to data corruption. }

type
  TSimple = record
    a: longint;
    b: string;     { should be ShortString = 256 bytes under $H- }
    c: longint;
  end;

var
  r: TSimple;
begin
  { SizeOf must reflect ShortString, not pointer }
  WriteLn('sizeof TSimple=', SizeOf(TSimple));
  WriteLn('sizeof string=', SizeOf(r.b));

  { Verify fields don't alias each other }
  r.a := 10;
  r.b := 'hello';
  r.c := 20;
  WriteLn('a=', r.a);
  WriteLn('b=', r.b);
  WriteLn('c=', r.c);
end.
