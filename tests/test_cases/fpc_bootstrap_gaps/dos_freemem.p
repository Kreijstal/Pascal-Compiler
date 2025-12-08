{
  Test: FreeMem procedure with optional size parameter
  
  FPC Behavior: FreeMem() can be called with 1 or 2 parameters:
    FreeMem(ptr)         - Free memory, size determined automatically
    FreeMem(ptr, size)   - Free memory with explicit size
  
  This is used extensively in dos.pp and other FPC RTL units.
  
  CRITICAL for FPC bootstrap: dos.pp uses this pattern.
}
program DosFreeMem;

var
  p: Pointer;
  
begin
  p := GetMem(100);
  
  { FPC supports both forms }
  FreeMem(p);       { One parameter form }
  
  p := GetMem(200);
  FreeMem(p, 200);  { Two parameter form }
  
  WriteLn('FreeMem test passed');
end.
