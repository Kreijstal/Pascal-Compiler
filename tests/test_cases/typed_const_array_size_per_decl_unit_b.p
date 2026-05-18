unit typed_const_array_size_per_decl_unit_b;

interface

implementation

uses typed_const_array_size_per_decl_recunit;

const
  { 4 entries of 4-byte trec2 = 16 bytes — DIFFERENT bound from unit_a. }
  sharedarr : array[0..3] of trec2 = (
    (u:20; c1:3; c2:4),
    (u:21; c1:3; c2:4),
    (u:22; c1:3; c2:4),
    (u:23; c1:3; c2:4)
  );

begin
  dummy(@sharedarr);
end.
