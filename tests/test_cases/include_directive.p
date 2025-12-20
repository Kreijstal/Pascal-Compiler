program IncludeDirective;

{$I ../test_data/includes/include_test_impl.inc}

begin
  TestInclude;
  WriteLn('Include directive test passed');
end.
