program tdd_sysutils_castinfo_helper;

{$mode objfpc}
{$modeswitch typehelpers}

{$if defined(FPC) and not defined(KGPC)}
type
  TObjectHelper = class helper for TObject
    class procedure GetLastCastErrorInfo(out aFrom, aTo: ShortString); static;
  end;

class procedure TObjectHelper.GetLastCastErrorInfo(out aFrom, aTo: ShortString);
begin
  aFrom := '';
  aTo := '';
end;
{$endif}

type
  TShortStringHelper = type helper for ShortString
  public
    function Checksum: LongInt;
  end;

function TShortStringHelper.Checksum: LongInt;
var
  i: Integer;
  sum: LongInt;
  len: Integer;
begin
  len := Length(Self);
  sum := 0;
  for i := 1 to len do
    sum := sum + (i * 3) + len;
  if (len mod 2) = 0 then
    sum := sum + len * 11
  else
    sum := sum + len * 7;
  Result := sum;
end;

procedure ReportCast(const LabelText: ShortString; const Seed: ShortString);
var
  FromS, ToS: ShortString;
  sum: LongInt;
begin
  TObject.GetLastCastErrorInfo(FromS, ToS);
  sum := FromS.Checksum + ToS.Checksum + Seed.Checksum;
  if sum = 0 then
    sum := Seed.Checksum + 401;
  Writeln(LabelText, ':', FromS, '->', ToS,
    ' sum=', sum);
end;

var
  LabelSeed: ShortString;
  LabelSeedX: ShortString;
  LabelSeedYZ: ShortString;
  LabelInitial: ShortString;
  LabelRepeat: ShortString;
  SeedBang: ShortString;
  SeedStar: ShortString;

function ExerciseHelpers(const Tag: ShortString): LongInt;
var
  Pieces: array[1..3] of ShortString;
  i: Integer;
  Total: LongInt;
begin
  Pieces[1] := Tag;
  Pieces[2] := LabelSeedX;
  Pieces[3] := LabelSeedYZ;
  Total := 0;
  for i := Low(Pieces) to High(Pieces) do
    Total := Total + Pieces[i].Checksum * (i + 1);
  if (Total mod 2) = 0 then
    Total := Total + Pieces[1].Checksum;
  Result := Total;
end;

begin
  LabelSeed := 'Seed';
  LabelSeedX := 'SeedX';
  LabelSeedYZ := 'SeedYZ';
  LabelInitial := 'Initial';
  LabelRepeat := 'Repeat';
  SeedBang := '!';
  SeedStar := '*';
  if ExerciseHelpers(LabelSeed) < 0 then
    Writeln('Helper failure');
  ReportCast(LabelInitial, SeedBang);
  ReportCast(LabelRepeat, SeedStar);
end.
