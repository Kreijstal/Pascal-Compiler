program PointerRecordDeref;
{ Test dereferencing typed pointers to records, including nested access
  and multi-field records. This exercises PMyRec = ^TMyRec resolution
  through the type alias chain. }
type
  TPoint = record
    x: Integer;
    y: Integer;
  end;
  PPoint = ^TPoint;

  TRect = record
    left: Integer;
    top: Integer;
    right: Integer;
    bottom: Integer;
  end;
  PRect = ^TRect;

var
  pt: TPoint;
  pp: PPoint;
  rc: TRect;
  pr: PRect;
begin
  pt.x := 10;
  pt.y := 20;
  pp := @pt;
  WriteLn(pp^.x);
  WriteLn(pp^.y);

  { Modify through pointer }
  pp^.x := 100;
  pp^.y := 200;
  WriteLn(pt.x);
  WriteLn(pt.y);

  rc.left := 1;
  rc.top := 2;
  rc.right := 3;
  rc.bottom := 4;
  pr := @rc;
  WriteLn(pr^.left);
  WriteLn(pr^.top);
  WriteLn(pr^.right);
  WriteLn(pr^.bottom);

  { Arithmetic on dereferenced fields }
  WriteLn(pp^.x + pp^.y);
  WriteLn(pr^.right - pr^.left);
end.
