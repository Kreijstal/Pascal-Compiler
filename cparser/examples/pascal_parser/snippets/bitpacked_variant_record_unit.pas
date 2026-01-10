unit bitpacked_variant_record_unit;

interface

type
  TByteBitIndex = 0..7;
  TByteNibbleIndex = 0..1;
  nibble = 0..15;
  TByteOverlay = bitpacked record case integer of
    0: (AsBit: bitpacked array[TByteBitIndex] of boolean);
    1: (AsNibble: bitpacked array[TByteNibbleIndex] of nibble);
    2: (AsByte: byte);
  end;

function AfterRecord: Integer;

implementation

function AfterRecord: Integer;
begin
  Result := 0;
end;

end.
