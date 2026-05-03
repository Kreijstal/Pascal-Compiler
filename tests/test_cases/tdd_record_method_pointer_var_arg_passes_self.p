program tdd_record_method_pointer_var_arg_passes_self;
{$mode objfpc}

type
  TCallbackResult = (CallbackOk, CallbackStop);
  TCallback = function(var Value: LongInt; Arg: Pointer): TCallbackResult of object;

  TAdapterContext = record
    Callback: TCallback;
    Arg: Pointer;
  end;

  TRunner = object
    Delta: LongInt;
    function Apply(var Value: LongInt; Arg: Pointer): TCallbackResult;
  end;

function TRunner.Apply(var Value: LongInt; Arg: Pointer): TCallbackResult;
begin
  Value := Value + Delta + PLongInt(Arg)^;
  Apply := CallbackStop;
end;

function RunAdapter(var Value: LongInt; Arg: Pointer): TCallbackResult;
var
  Context: ^TAdapterContext absolute Arg;
begin
  RunAdapter := Context^.Callback(Value, Context^.Arg);
end;

var
  Runner: TRunner;
  Context: TAdapterContext;
  Value: LongInt;
  Extra: LongInt;

begin
  Runner.Delta := 7;
  Extra := 5;
  Value := 3;
  Context.Callback := @Runner.Apply;
  Context.Arg := @Extra;
  writeln(Ord(RunAdapter(Value, @Context)));
  writeln(Value);
end.
