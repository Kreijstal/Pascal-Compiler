unit cthreads;

{ cthreads unit - provides threading support }

interface

{ Returns TRUE if threading functions have already been used, FALSE otherwise.
  Since this implementation doesn't actually start threads, it always returns FALSE. }
function ThreadingAlreadyUsed: Boolean;

implementation

function ThreadingAlreadyUsed: Boolean;
begin
  Result := FALSE;
end;

end.
