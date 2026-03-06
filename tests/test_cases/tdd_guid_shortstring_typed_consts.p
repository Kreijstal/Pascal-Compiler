program tdd_guid_shortstring_typed_consts;

{$mode objfpc}
{$H-}

const
  G: TGuid = '{D91C9AF4-3C93-420F-A303-BF5BA82BFD23}';
  S: ShortString = 'UTF-8';

begin
  Writeln(Length(S));
end.
