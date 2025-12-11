{ This test mirrors the conditional SIZEOF(ValReal) checks used by FPC's system unit. }
program ValRealSizeofDirective;
begin
  {$if SizeOf(ValReal)=10}
  writeln('ValReal size=10');
  {$elseif SizeOf(ValReal)=8}
  writeln('ValReal size=8');
  {$else}
  writeln('ValReal size=', SizeOf(ValReal));
  {$endif}
end.
