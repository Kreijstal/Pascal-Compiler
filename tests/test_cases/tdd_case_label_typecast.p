program TDDCaseLabelTypecast;

type
  size_t = Longint;

var
  mblen: size_t;

begin
  mblen := size_t(-2);
  case mblen of
    size_t(-2): writeln(1);
    size_t(-1), 0: writeln(2);
  else
    writeln(3);
  end;
end.
