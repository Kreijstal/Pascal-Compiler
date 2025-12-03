// Full Unit code.
// -----------------------------------------------------------
// You must store this code in a unit called Unit1 with a form
// called Form1 that has an OnCreate event called FormCreate.

unit Unit1;

interface         // Defines the external view of this unit

uses
  Forms;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

Implementation   // Implements the Interface of this unit
{$R *.dfm}       // Include form definitions

uses             // Private units
  Dialogs;

var              // Private variables
  msg : string;

const            // Private constants
  MSG_TEXT = 'Hello World';

// A private routine - not predefined in the Interface section
procedure SayHello;
begin
  // Say hello to the World
  msg := MSG_TEXT;
  ShowMessage(msg);
end;

// A routine pre-defined in the Interface section
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Say hello
  SayHello;
end;

end.
