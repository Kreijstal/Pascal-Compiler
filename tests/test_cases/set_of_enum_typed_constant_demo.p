program SetOfEnumTypedConstantDemo;

uses set_of_enum_typed_constant;

begin
    if foReadOnly in DefaultOptions then
        writeln('readonly');

    if foHidden in DefaultOptions then
        writeln('hidden')
    else
        writeln('visible');

    if foSystem in DefaultOptions then
        writeln('system');
end.
