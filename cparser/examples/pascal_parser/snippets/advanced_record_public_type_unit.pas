unit adv_record_public_type;

interface

type
  TOSVersion = record
  public type
    TArchitecture = (arIntelX86, arIntelX64);
    TPlatform = (pfWindows, pfLinux);
  public
    Version: LongInt;
  end;

function AfterRecord: Integer;

implementation

function AfterRecord: Integer;
begin
  Result := 0;
end;

end.
