unit AdvancedRecordPrivateTypeObjectDirectives;
{$mode delphi}

interface

type
  TMarshaller = record
  private type
    TBase = object
      constructor Init;
      destructor Done; virtual; abstract;
    end;

    TChild = object(TBase)
      destructor Done; virtual;
    end;

    function PushDefer: Integer;
  var
    FState: Integer;
  public
    procedure Flush;
  end;

implementation

function TMarshaller.PushDefer: Integer;
begin
  Result := FState;
end;

procedure TMarshaller.Flush;
begin
end;

end.
