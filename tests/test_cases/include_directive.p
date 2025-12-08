program IncludeDirective;

{$I include_test_impl.inc}

begin
  TestInclude;
  WriteLn('Include directive test passed');
end.
