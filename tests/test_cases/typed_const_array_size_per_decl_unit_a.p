unit typed_const_array_size_per_decl_unit_a;

interface

implementation

uses typed_const_array_size_per_decl_recunit;

const
  { 8 entries of 4-byte trec2 = 32 bytes. }
  sharedarr : array[0..7] of trec2 = (
    (u:10; c1:1; c2:2),
    (u:11; c1:1; c2:2),
    (u:12; c1:1; c2:2),
    (u:13; c1:1; c2:2),
    (u:14; c1:1; c2:2),
    (u:15; c1:1; c2:2),
    (u:16; c1:1; c2:2),
    (u:17; c1:1; c2:2)
  );

begin
  dummy(@sharedarr);
end.
