program TestForward;
 procedure Second; forward;
 
 procedure First;
 begin
 writeln('First, calling Second');
 Second;
 end;
 
 procedure Second;
 begin
 writeln('Second');
 end;
 
 begin
 First;
 end.
