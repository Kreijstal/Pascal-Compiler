unit multiline;

interface

implementation

const
  { Simple strings with escaped quotes }
  Str1 = 'Str''Str';
  Str2 = '';

  { Strings with various escape patterns }
  Str3 = '''TEST STRING''';    { String containing 'TEST STRING' }
  Str4 = '''''TEST''''';       { String containing ''TEST'' }
  Str5 = '''''''TEST''''''';   { String containing '''TEST''' }

  { Using concatenation for multiline effect }
  MultiLine1 = 'First line' + #13#10 + 'Second line';
  MultiLine2 = 'Line 1' +
               'Line 2' +
               'Line 3';
end.
