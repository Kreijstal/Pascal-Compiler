unit typed_const_cross_unit_shortstring_array_recunit;

interface

type
  ttinytok = (ttt_zero, ttt_one, ttt_two, ttt_three);

const
  { Element type is `string[10]` — 11-byte storage per element.
    Declared in this unit so cross-unit init code is exercised when
    consumed by the program below. }
  tok2str : array[ttinytok] of string[10] = (
    'zero',
    'one',
    'two',
    'three'
  );

implementation

end.
