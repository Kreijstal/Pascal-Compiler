unit set_of_enum_typed_constant;

interface

type
  TFileOption = (foReadOnly, foHidden, foSystem);
const
  DefaultOptions: set of TFileOption = [foReadOnly, foSystem];

implementation

end.
