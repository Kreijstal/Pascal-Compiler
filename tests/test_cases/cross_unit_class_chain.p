program cross_unit_class_chain;
{ Test chained field access through cross-unit class types.
  unit_c impl-uses unit_b which iface-uses unit_a.
  Accessing current.inner.FLevel requires resolving TInner from unit_a
  even though unit_c doesn't directly use unit_a. }
uses cross_unit_class_chain_mid;
begin
  current := TProcInfo.Create;
  WriteLn(current.inner.FLevel);
  current.inner.Free;
  current.Free;
end.
