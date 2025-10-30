program RecordReferenceFeatures;

type
  SimpleRecord = record
    alpha: Integer;
    beta: Integer;
    count: Integer;
  end;
  PInteger = ^Integer;

var
  globalRec: SimpleRecord;
  otherRec: SimpleRecord;
  arr: array[0..3] of Integer;
  valuePtr: PInteger;
  i: Integer;
  computed: Integer;

begin
  for i := 0 to 3 do
    arr[i] := i * 10;

  globalRec.alpha := 7;
  globalRec.beta := 8;
  globalRec.count := 9;

  otherRec.alpha := 3;
  otherRec.beta := 4;
  otherRec.count := 5;

  inc(globalRec.alpha, 5);
  valuePtr := @globalRec.beta;
  inc(valuePtr^, 2);

  computed := arr[succ(0)] + arr[succ(succ(0))];
  inc(otherRec.count, computed div 2);
  inc(otherRec.count, succ(arr[succ(0)]));

  valuePtr := @otherRec.alpha;
  inc(valuePtr^, arr[succ(0)]);

  globalRec.count := globalRec.count + computed;

  writeln(globalRec.alpha);
  writeln(globalRec.beta);
  writeln(globalRec.count);
  writeln(otherRec.alpha);
  writeln(otherRec.count);

  valuePtr := @arr[succ(1)];
  inc(valuePtr^, globalRec.alpha);
  writeln(arr[succ(1)]);

  otherRec := globalRec;
  writeln(otherRec.alpha);
  writeln(otherRec.beta);
  writeln(otherRec.count);
end.
