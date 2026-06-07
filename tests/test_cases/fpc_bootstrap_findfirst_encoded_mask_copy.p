{$H+}
program fpc_bootstrap_findfirst_encoded_mask_copy;

const
  CP_UTF8 = 65001;

function EncodePath(const Str: RawByteString): RawByteString;
begin
  EncodePath := Str;
  SetCodePage(EncodePath, CP_UTF8, True);
end;

var
  SearchSpec, SName: RawByteString;
  NamePos: LongInt;

begin
  SearchSpec := EncodePath('/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/*');
  NamePos := Length(SearchSpec);
  while (NamePos > 0) and (SearchSpec[NamePos] <> '/') do
    dec(NamePos);
  SName := Copy(SearchSpec, NamePos + 1, Length(SearchSpec));
  Writeln('spec-len=', Length(SearchSpec));
  Writeln('name-pos=', NamePos);
  Writeln('mask-len=', Length(SName));
  Writeln(SName);
end.
