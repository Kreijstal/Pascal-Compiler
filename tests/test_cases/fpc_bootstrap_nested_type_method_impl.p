{
  Test case: Nested type method implementation with triple-dotted name.

  BUG: The PEG parser's method_name_with_class grammar only supports
  single-dot names (Class.Method) but not nested type method implementations
  (Class.InnerType.Method). When it encounters a triple-dotted name like
  "constructor TOuter.TInner.Create", parsing fails and ALL subsequent
  declarations in the implementation section are lost (become a NONE node
  with raw text). This causes any standalone procedure after the nested
  type method to be reported as "not declared".

  This blocks FPC bootstrap because sysutils.pp has TMarshaller.TState.Create
  and other nested type methods, causing InitExceptions to be lost.
}
program fpc_bootstrap_nested_type_method_impl;
{$mode objfpc}

type
  TOuter = class
  type
    TInner = class
      constructor Create;
    end;
  end;

constructor TOuter.TInner.Create;
begin
end;

procedure DoStuff;
begin
  writeln('OK');
end;

begin
  DoStuff;
end.
