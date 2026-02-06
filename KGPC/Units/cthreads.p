unit cthreads;

{ cthreads unit - provides threading support }

interface

{ Returns TRUE if threading functions have already been used, FALSE otherwise.
  Set by the runtime when cthreads initializes. }
function ThreadingAlreadyUsed: Boolean; cdecl; external name 'threadingalreadyused_void';
procedure kgpc_cthreads_init; cdecl; external name 'kgpc_cthreads_init';

implementation

initialization
  kgpc_cthreads_init;
  IsMultiThread := True;

end.
