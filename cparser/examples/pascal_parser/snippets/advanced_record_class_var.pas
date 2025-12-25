{$mode objfpc}
{$modeswitch advancedrecords}

program TestAdvancedRecordClassVar;
type
  TAdvRecord = record
  public
    class var FInstance: Integer;
    Value: Integer;
  end;
begin
end.
