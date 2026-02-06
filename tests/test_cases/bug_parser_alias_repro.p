program bug_parser_alias_repro;

{ objfpc}

procedure TestAlias; [public, alias: 'TEST_ALIAS'];
var
  i: Integer;
begin
  i := 1;
end;

procedure TestNext;
begin
end;

begin
  TestNext;
  Writeln('OK');
end.
