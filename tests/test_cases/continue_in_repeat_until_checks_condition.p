{ Regression: `Continue` inside a `repeat..until` loop must branch to the
  until-condition test, not the loop-body top.  KGPC compiled it as a jump to
  the loop top, so a Continue taken on the final pass skipped the termination
  test and the loop over-ran by (at least) one iteration -- or never terminated.

  This miscompiled FPC's own assembler peephole optimiser (StripDeadLabels'
  `repeat with tai_label(tmp).labsym ... if not(labeltype in [...]) then begin
  tmp:=tmp.Next; Continue end; ... until (tmp=BlockEnd) or (tmp.typ<>ait_label)`):
  the Continue skipped the `until`, walking `tmp` past the last label into a
  tai_regalloc whose `labsym` was then dereferenced as garbage -> SIGSEGV in the
  KGPC-built FPC compiler (pp_bootstrap) at -O2, only reachable once PeepHoleOptPass2
  ran. Expected i=3 (Continue at i=3 evaluates `until i>=3` and exits). }
program continue_in_repeat_until_checks_condition;
var i, j, inner: longint;
begin
  i := 0;
  repeat
    i := i + 1;
    if odd(i) then Continue;
  until i >= 3;
  writeln('repeat_i=', i);            { 3, not 4 }

  { nested: Continue affects only the innermost repeat }
  inner := 0;
  i := 0;
  repeat
    i := i + 1;
    j := 0;
    repeat
      j := j + 1;
      if odd(j) then Continue;
      inner := inner + 1;
    until j >= 4;
  until i >= 2;
  writeln('outer_i=', i, ' inner_hits=', inner);  { i=2, inner=4 (j=2,4 per outer pass x2) }
end.
