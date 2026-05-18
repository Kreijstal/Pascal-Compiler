program RefcountSharedObjectFreeVmtDispatch;

{ Regression test: a subclass that overrides FreeInstance to implement
  refcount-shared semantics must be able to receive multiple Free calls
  on the same object without the storage being released until the refcount
  reaches zero.  Before the fix the emitted body of TObject.Free called
  kgpc_freemem unconditionally — bypassing the overridden FreeInstance and
  freeing the storage on the very first Free.  After the fix Free dispatches
  to FreeInstance through the VMT, which honours subclasses' overrides. }

{$mode objfpc}

uses
  objpas;

type
  TRefcountedThing = class(TObject)
  private
    FRefCount: Integer;
  public
    Tag: Integer;
    constructor Create(aTag: Integer);
    destructor Destroy; override;
    procedure FreeInstance; override;
    procedure Share;
  end;

constructor TRefcountedThing.Create(aTag: Integer);
begin
  inherited Create;
  Tag := aTag;
  FRefCount := 1;
end;

destructor TRefcountedThing.Destroy;
begin
  if FRefCount > 1 then
    Exit;
  WriteLn('destroy:', Tag);
  inherited Destroy;
end;

procedure TRefcountedThing.FreeInstance;
begin
  Dec(FRefCount);
  if FRefCount = 0 then
    inherited FreeInstance;
end;

procedure TRefcountedThing.Share;
begin
  Inc(FRefCount);
end;

var
  Obj: TRefcountedThing;
begin
  Obj := TRefcountedThing.Create(7);
  Obj.Share;       { refcount = 2 }
  Obj.Free;        { refcount = 1, must NOT release storage }
  WriteLn('mid:', Obj.Tag);
  Obj.Free;        { refcount = 0, releases storage }
  WriteLn('done');
end.
