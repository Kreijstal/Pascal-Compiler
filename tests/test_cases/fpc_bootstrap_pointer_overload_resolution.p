{$mode objfpc}
program fpc_bootstrap_pointer_overload_resolution;

type
  TKind = (kRaw, kShort, kPChar);

function Pick(const S: RawByteString): TKind; overload;
begin
  Result := kRaw;
end;

function Pick(const S: ShortString): TKind; overload;
begin
  Result := kShort;
end;

function Pick(var S: PAnsiChar): TKind; overload;
begin
  Result := kPChar;
end;

function OpenFile(path: PAnsiChar; flags: LongInt): LongInt; overload;
begin
  Result := 1;
end;

function OpenFile(const path: RawByteString; flags: LongInt): LongInt; overload;
begin
  Result := 2;
end;



function KindToStr(K: TKind): string;
begin
  case K of
    kRaw: Result := 'raw';
    kShort: Result := 'short';
    kPChar: Result := 'pchar';
  end;
end;

var
  raw: RawByteString;
  ss: ShortString;
  k1, k2, k3: TKind;
  checksum: Integer;
  openResult: LongInt;
begin
  raw := 'alpha';
  ss := 'beta';

  k1 := Pick(raw);
  k2 := Pick(ss);
  k3 := Pick(PAnsiChar(pointer(raw)));

  openResult := OpenFile(pointer(raw), 7);

  checksum := Ord(k1) * 10 + Ord(k2) * 3 + Ord(k3);
  writeln('k1=', KindToStr(k1));
  writeln('k2=', KindToStr(k2));
  writeln('k3=', KindToStr(k3));
  writeln('checksum=', checksum);
  writeln('open=', openResult);
end.
