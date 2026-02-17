{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
program test_proc_field_dispatch;

{ Test: invoking procedural-type fields on record variables
  within function/procedure bodies (not just at program level).
  This exercises the method_call_placeholder -> procedural field
  fallback in the semantic checker. }

type
  TInitProc = procedure;
  TFinalProc = procedure;
  TTransformFunc = function(x: Integer): Integer;
  TVarOpProc = procedure(var dest: Integer; src: Integer);

  TInitFinalRec = record
    InitProc: TInitProc;
    FinalProc: TFinalProc;
  end;

  TRTTIRecVarOp = procedure(ARec: Pointer);
  TRTTIRecCopyOp = procedure(ASrc, ADest: Pointer);

  TRTTIRecordOpVMT = record
    case Integer of
      0: (
        Initialize: TRTTIRecVarOp;
        Finalize: TRTTIRecVarOp;
        AddRef: TRTTIRecVarOp;
        Copy: TRTTIRecCopyOp;
      );
  end;

  TCallbackManager = record
    Transform: TTransformFunc;
    VarOp: TVarOpProc;
  end;

var
  globalVal: Integer;
  globalStr: String;

procedure InitHandler;
begin
  globalStr := globalStr + 'INIT;';
end;

procedure FinalHandler;
begin
  globalStr := globalStr + 'FINAL;';
end;

procedure MyInit(ARec: Pointer);
begin
  globalVal := globalVal + 100;
end;

procedure MyFinal(ARec: Pointer);
begin
  globalVal := globalVal + 200;
end;

procedure MyCopy(ASrc, ADest: Pointer);
begin
  globalVal := globalVal + 500;
end;

function DoubleIt(x: Integer): Integer;
begin
  DoubleIt := x * 2;
end;

procedure AddTo(var dest: Integer; src: Integer);
begin
  dest := dest + src;
end;

{ Key test: calling procedural fields inside a procedure body }
procedure RunInitFinal(rec: TInitFinalRec);
begin
  rec.InitProc;
  rec.FinalProc;
end;

procedure RunVMTOps(var vmt: TRTTIRecordOpVMT);
begin
  vmt.Initialize(nil);
  vmt.Finalize(nil);
  vmt.Copy(nil, nil);
end;

function ApplyTransform(mgr: TCallbackManager; value: Integer): Integer;
begin
  ApplyTransform := mgr.Transform(value);
end;

procedure ApplyVarOp(mgr: TCallbackManager; var dest: Integer; src: Integer);
begin
  mgr.VarOp(dest, src);
end;

var
  ifr: TInitFinalRec;
  vmt: TRTTIRecordOpVMT;
  mgr: TCallbackManager;
  result, accum: Integer;
begin
  { Test 1: InitFinalRec procedural fields }
  globalStr := '';
  ifr.InitProc := @InitHandler;
  ifr.FinalProc := @FinalHandler;
  RunInitFinal(ifr);
  WriteLn('Test1: ', globalStr);  { expect: INIT;FINAL; }

  { Test 2: Variant record VMT procedural fields }
  globalVal := 0;
  vmt.Initialize := @MyInit;
  vmt.Finalize := @MyFinal;
  vmt.Copy := @MyCopy;
  RunVMTOps(vmt);
  WriteLn('Test2: ', globalVal);  { expect: 800 (100+200+500) }

  { Test 3: Function-type field returning a value }
  mgr.Transform := @DoubleIt;
  mgr.VarOp := @AddTo;
  result := ApplyTransform(mgr, 21);
  WriteLn('Test3: ', result);  { expect: 42 }

  { Test 4: Var-param procedural field }
  accum := 10;
  ApplyVarOp(mgr, accum, 5);
  WriteLn('Test4: ', accum);  { expect: 15 }
end.
