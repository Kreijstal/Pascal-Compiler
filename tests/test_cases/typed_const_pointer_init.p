program typed_const_pointer_init;

type
  PStr = ^string;

const
  EmptyStr: string = '';
  NullStr: PStr = @EmptyStr;

begin
  if NullStr = @EmptyStr then
    WriteLn('ok')
  else
    WriteLn('bad');
end.
