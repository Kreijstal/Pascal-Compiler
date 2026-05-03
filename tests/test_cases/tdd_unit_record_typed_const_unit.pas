{ Helper unit for tdd_unit_record_typed_const.

  Defines a record type that is then used as a typed-const target in another
  unit's IMPLEMENTATION section.  The bug being pinned: when a typed const of
  this record is declared inside another unit's implementation (so the record
  type's defining unit is only an implementation-private dependency from the
  outer program's view), KGPC's `collect_typed_const_decls_filtered` would
  drop the decl because the record type isn't reachable from the program's
  scope.  As a result the field-by-field initializer assignments never had
  their `field_offset` resolved, leaving every store at offset 0 — every
  field of the typed const ended up overwriting `(typed_const + 0)`. }

unit tdd_unit_record_typed_const_unit;

interface

type
  TKind = (kind_a, kind_b, kind_c, kind_d, kind_e);
  TBuf  = array[TKind] of byte;
  TFnA  = function(arg: byte): byte;
  TFnB  = procedure(arg: pointer);

  TFoo = record
    page_size: longword;
    image_base: longword;
    code: word;
    flag: boolean;
    bytes: TBuf;
    fa: TFnA;
    fb: TFnB;
    p1: pointer;
    p2: pointer;
  end;

implementation

end.
