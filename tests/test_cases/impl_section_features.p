{ Test unit with implementation section features:
  - Private uses clause
  - Private var section  
  - Private const section
  - Private procedure
}
unit impl_section_features;

interface

procedure PublicProcedure;

implementation

uses
  SysUtils;    // Private uses clause (even if not used, tests parsing)

var              // Private variables
  msg: string;
  counter: integer;

const            // Private constants
  MSG_TEXT = 'Hello from implementation';
  MAX_COUNT = 10;

// A private routine - not predefined in the Interface section
procedure PrivateProcedure;
begin
  msg := MSG_TEXT;
  counter := counter + 1;
end;

// A routine pre-defined in the Interface section
procedure PublicProcedure;
begin
  PrivateProcedure;
  WriteLn(msg);
  WriteLn('Counter: ', counter);
end;

initialization
  counter := 0;

end.
