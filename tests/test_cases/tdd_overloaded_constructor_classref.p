{ Regression test: classref.create(args) with overloaded constructors

  Two virtual constructors share name+param_count but differ in arg types:
    TLoop.Create(tag: integer; a, b, c, d: TObject);
    TFor.Create(a, b, c, d: TObject; back: boolean);

  TFor inherits from TLoop and overloads create. The VMT lookup must
  pick TFor.Create when the call has 4 objects + boolean, not TLoop.Create
  (which has 1 integer + 4 objects). Before the fix, both virtual VMT
  resolvers picked by name+param_count alone, returning the inherited
  TLoop.Create slot. This is exactly the FPC tloopnode/tfornode bug that
  blocked pp.pas stage-4 self-host.

  Symptom of the bug: the wrong constructor receives shifted args, leaving
  the instance's fields uninitialized — manifests later as "Incompatible
  types: got 'untyped'..." once typecheck walks the misaligned tree. }
{$mode objfpc}
program tdd_overloaded_constructor_classref;

type
  TLoop = class
    sentinel: integer;
    constructor Create(tag: integer; a, b, c, d: TObject); overload; virtual;
    constructor Create(a, b, c, d: TObject; back: boolean); overload; virtual;
  end;

  TFor = class(TLoop)
  end;

  TLoopClass = class of TLoop;

constructor TLoop.Create(tag: integer; a, b, c, d: TObject);
begin
  sentinel := 100 + tag;
end;

constructor TLoop.Create(a, b, c, d: TObject; back: boolean);
begin
  if back then sentinel := 200 else sentinel := 300;
end;

var
  cf: TLoopClass;
  inst: TLoop;
begin
  cf := TFor;
  inst := cf.Create(nil, nil, nil, nil, true);
  writeln('via classref boolean overload: ', inst.sentinel);
  inst.Free;
  inst := cf.Create(7, nil, nil, nil, nil);
  writeln('via classref integer overload: ', inst.sentinel);
  inst.Free;
end.
