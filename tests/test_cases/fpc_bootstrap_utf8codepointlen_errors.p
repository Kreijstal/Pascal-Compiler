{$H+}
program fpc_bootstrap_utf8codepointlen_errors;

function CodePointLen(const S: RawByteString; IncludePartial: Boolean): SizeInt;
begin
  CodePointLen := Utf8CodePointLen(PAnsiChar(@S[1]), Length(S), IncludePartial);
end;

var
  S: RawByteString;

begin
  S := 'A';
  Writeln('ascii=', CodePointLen(S, True));

  S := Chr($E2) + Chr($82) + Chr($AC);
  Writeln('euro=', CodePointLen(S, True));

  S := Chr($80);
  Writeln('invalid-start=', CodePointLen(S, True));

  S := Chr($E2) + Chr($82);
  Writeln('partial-true=', CodePointLen(S, True));
  Writeln('partial-false=', CodePointLen(S, False));

  S := Chr($E2) + Chr($28) + Chr($A1);
  Writeln('bad-continuation=', CodePointLen(S, True));

  S := Chr($E0) + Chr($80) + Chr($80);
  Writeln('overlong=', CodePointLen(S, True));
end.
