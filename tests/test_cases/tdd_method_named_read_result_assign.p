program tdd_method_named_read_result_assign;

type
  TBuf = class
    function Read: Integer;
  end;

function TBuf.Read: Integer;
begin
  Read := 41;
  Inc(Read);
end;

begin
  Writeln('ok');
end.
