{$mode objfpc}
{$modeswitch advancedrecords}
program fpc_bootstrap_record_classvars;

type
  TVersionInfo = record
  private
    class var FMajor: Integer;
    class var FMinor: Integer;
    class var FServicePack: Integer;
    class var FFull: string;
    class function Init: TVersionInfo; static;
    class function GetFull: string; static;
  public
    class function Check(AMajor: Integer): Boolean; overload; static; inline;
    class function Check(AMajor, AMinor: Integer): Boolean; overload; static; inline;
    class function Check(AMajor, AMinor, AServicePack: Integer): Boolean; overload; static; inline;
    class property Major: Integer read FMajor;
    class property Minor: Integer read FMinor;
    class property ServicePack: Integer read FServicePack;
    class property Full: string read GetFull;
  end;

class function TVersionInfo.Init: TVersionInfo;
begin
  FMajor := 10;
  FMinor := 2;
  FServicePack := 5;
  FFull := '10.2.5';
  Result.FMajor := FMajor;
  Result.FMinor := FMinor;
  Result.FServicePack := FServicePack;
  Result.FFull := FFull;
end;

class function TVersionInfo.GetFull: string;
begin
  Result := FFull;
end;

class function TVersionInfo.Check(AMajor: Integer): Boolean; overload; static; inline;
begin
  Result := (Major >= AMajor);
end;

class function TVersionInfo.Check(AMajor, AMinor: Integer): Boolean; overload; static; inline;
begin
  Result := (Major > AMajor) or ((Major = AMajor) and (Minor >= AMinor));
end;

class function TVersionInfo.Check(AMajor, AMinor, AServicePack: Integer): Boolean; overload; static; inline;
begin
  Result := (Major > AMajor)
    or ((Major = AMajor) and (Minor > AMinor))
    or ((Major = AMajor) and (Minor = AMinor) and (ServicePack >= AServicePack));
end;

var
  score: Integer;
  initState: TVersionInfo;
begin
  initState := TVersionInfo.Init;
  score := 0;
  if TVersionInfo.Check(9) then Inc(score, 1);
  if TVersionInfo.Check(10) then Inc(score, 2);
  if TVersionInfo.Check(10, 1) then Inc(score, 4);
  if TVersionInfo.Check(10, 2) then Inc(score, 8);
  if TVersionInfo.Check(10, 2, 4) then Inc(score, 16);
  if TVersionInfo.Check(10, 2, 5) then Inc(score, 32);
  if not TVersionInfo.Check(11) then Inc(score, 64);
  if not TVersionInfo.Check(10, 3) then Inc(score, 128);

  writeln('major=', TVersionInfo.Major);
  writeln('minor=', TVersionInfo.Minor);
  writeln('sp=', TVersionInfo.ServicePack);
  writeln('full=', TVersionInfo.Full);
  writeln('score=', score);
end.
