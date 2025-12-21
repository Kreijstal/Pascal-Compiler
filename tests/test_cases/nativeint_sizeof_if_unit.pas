unit nativeint_sizeof_if_unit;

{$mode objfpc}

interface

{$if sizeof(NativeInt)=8}
const
  WordSize = 8;
{$elseif sizeof(NativeInt)=4}
const
  WordSize = 4;
{$else}
const
  WordSize = 0;
{$endif}

implementation

end.
