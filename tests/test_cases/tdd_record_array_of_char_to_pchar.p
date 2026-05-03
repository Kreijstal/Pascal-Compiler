{ Regression test for codegen bug:
  When a record field of type `array[0..N] of char` is assigned to a `pchar`
  variable, KGPC was emitting `mov[lq] (%addr), %reg` (dereferencing the field
  contents as a primitive) instead of `mov %addr, %reg` (decaying the array
  to its address). The result was that codes/pchar received the first
  4 bytes of the array bytes interpreted as a pointer, which then
  segfaulted when dereferenced.

  This was the root cause of the SIGSEGV in pp_bootstrap's taicpu.gencode
  at the line `codes := insentry^.code`.
}
program tdd_record_array_of_char_to_pchar;
type
  trec = record
    n: longint;
    code: array[0..12] of char;
  end;
  prec = ^trec;
var
  rec: trec;
  pr: prec;
  p: pchar;
begin
  rec.code[0] := 'A';
  rec.code[1] := 'B';
  rec.code[2] := #0;
  pr := @rec;
  { This is the buggy case: array-of-char field decay to pchar via pointer-to-record. }
  p := pr^.code;
  writeln(p);
end.
