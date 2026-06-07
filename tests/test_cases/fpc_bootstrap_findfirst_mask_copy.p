{$H+}
program fpc_bootstrap_findfirst_mask_copy;

const
  AllFilesMask = '*';

var
  SearchSpec, SName: RawByteString;
  NamePos: LongInt;

begin
  SearchSpec := './' + AllFilesMask;
  NamePos := 2;
  SName := Copy(SearchSpec, NamePos + 1, Length(SearchSpec));
  Writeln('spec-len=', Length(SearchSpec));
  Writeln('mask-len=', Length(SName));
  Writeln(SName);
end.
