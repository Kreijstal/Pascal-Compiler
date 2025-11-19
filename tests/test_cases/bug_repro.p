program BugRepro;

type
  TMyRecord = record
    ID: Integer;
    Name: string;
  end;

  generic TFPGList<_T> = class
  private
    FItems: array of _T;
  public
    procedure Add(const Item: _T);
  end;

  TMyRecordList = specialize TFPGList<TMyRecord>;

  TMyObject = class
  end;

procedure FreeAndNil(var Obj: Pointer);
begin
  TObject(Obj).Free;
  Obj := nil;
end;

var
  ListSize: Integer;
  MyObj: TMyObject;

begin
  ListSize := SizeOf(TMyRecordList);
  WriteLn('SizeOf(TMyRecordList): ', ListSize);

  MyObj := TMyObject.Create;
  FreeAndNil(MyObj);
  if MyObj = nil then
    WriteLn('MyObj is nil')
  else
    WriteLn('MyObj is not nil');
end.
