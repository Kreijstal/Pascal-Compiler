program tdd_sysutils_enum_low_high_set;

{$mode objfpc}

type
  TArch = (arAlpha, arBeta, arGamma, arDelta, arEpsilon);
  TPlat = (pfWin, pfMac, pfLinux, pfBSD);
  TArchSet = set of TArch;
  TPlatSet = set of TPlat;

const
  AllArch = [Low(TArch)..High(TArch)];
  AllPlat = [Low(TPlat)..High(TPlat)];
  CoreArch = [arAlpha, arGamma, arEpsilon];
  CorePlat = [pfWin, pfLinux];

type
  TSummary = record
    ArchScore: LongInt;
    PlatScore: LongInt;
    MixScore: LongInt;
  end;

function RotateLeft(Value: LongInt; Bits: Integer): LongInt;
begin
  Result := (Value shl Bits) or (Value shr (32 - Bits));
end;

function BuildSummary(ArchMask: TArchSet; PlatMask: TPlatSet): TSummary;
var
  Arch: TArch;
  Plat: TPlat;
  Factor: LongInt;
  MixedArch: TArchSet;
  MixedPlat: TPlatSet;
  Cross: LongInt;
begin
  Result.ArchScore := 0;
  Result.PlatScore := 0;
  Result.MixScore := 0;

  Factor := 3;
  for Arch := arAlpha to arEpsilon do
  begin
    if Arch in ArchMask then
    begin
      Result.ArchScore := Result.ArchScore + (Ord(Arch) + 1) * Factor;
      Factor := Factor + 2;
    end
    else
      Factor := Factor + 1;
  end;

  Factor := 5;
  for Plat := pfWin to pfBSD do
  begin
    if Plat in PlatMask then
    begin
      Result.PlatScore := Result.PlatScore + (Ord(Plat) + 2) * Factor;
      Factor := Factor + 3;
    end
    else
      Factor := Factor + 1;
  end;

  MixedArch := (ArchMask + CoreArch) - [arBeta];
  MixedPlat := (PlatMask * AllPlat) + CorePlat;
  Cross := 0;
  for Arch := arAlpha to arEpsilon do
    for Plat := pfWin to pfBSD do
      if (Arch in MixedArch) and (Plat in MixedPlat) then
        Cross := Cross + (Ord(Arch) + 1) * (Ord(Plat) + 4);

  Result.MixScore := RotateLeft(Result.ArchScore + Result.PlatScore + Cross, 3);
end;

function ComputeChecksum: LongInt;
var
  FullSummary: TSummary;
  CoreSummary: TSummary;
begin
  FullSummary := BuildSummary(AllArch, AllPlat);
  CoreSummary := BuildSummary(CoreArch, CorePlat);
  Result := FullSummary.ArchScore * 7 +
            FullSummary.PlatScore * 5 +
            FullSummary.MixScore * 3 +
            CoreSummary.ArchScore * 11 +
            CoreSummary.PlatScore * 13 +
            CoreSummary.MixScore * 17;
end;

var
  Checksum: LongInt;
begin
  Checksum := ComputeChecksum;
  WriteLn('Checksum=', Checksum);
end.
