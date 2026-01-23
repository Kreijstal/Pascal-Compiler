unit cthreads;

{ cthreads unit - provides threading support }

interface

{ Returns TRUE if threading functions have already been used, FALSE otherwise.
  Set by the runtime when cthreads initializes. }
function ThreadingAlreadyUsed: Boolean;

implementation

procedure kgpc_cthreads_init; cdecl; external name 'kgpc_cthreads_init';
function kgpc_threading_already_used: LongInt; cdecl; external name 'kgpc_threading_already_used';

function ThreadingAlreadyUsed: Boolean;
begin
  Result := kgpc_threading_already_used <> 0;
end;

initialization
  kgpc_cthreads_init;
  IsMultiThread := True;

end.
