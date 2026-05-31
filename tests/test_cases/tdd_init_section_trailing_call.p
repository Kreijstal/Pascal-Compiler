program tdd_init_section_trailing_call;
uses tdd_init_section_trailing_call_u;
begin
  { If the trailing init call setm(42) was dropped, marker stays 1. }
  writeln(marker);
end.
