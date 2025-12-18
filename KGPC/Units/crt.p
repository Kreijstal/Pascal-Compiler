unit Crt;

interface

const
    Black = 0;
    Blue = 1;
    Green = 2;
    Cyan = 3;
    Red = 4;
    Magenta = 5;
    Brown = 6;
    LightGray = 7;
    DarkGray = 8;
    LightBlue = 9;
    LightGreen = 10;
    LightCyan = 11;
    LightRed = 12;
    LightMagenta = 13;
    Yellow = 14;
    White = 15;

procedure clrscr;
procedure clreol;
procedure gotoxy(x, y: integer);
procedure window(x1, y1, x2, y2: integer);
procedure textcolor(color: integer);
procedure textbackground(color: integer);
procedure highvideo;
procedure lowvideo;
procedure normvideo;
function readkey: char;
function screenwidth: integer;
function screenheight: integer;

implementation

function kgpc_keyboard_read_char: integer; external;
function kgpc_crt_screen_width: integer; external;
function kgpc_crt_screen_height: integer; external;

var
    crt_win_x1: integer = 1;
    crt_win_y1: integer = 1;
    crt_win_x2: integer = 80;
    crt_win_y2: integer = 25;
    crt_cur_x: integer = 1;
    crt_cur_y: integer = 1;

procedure crt_gotoxy_abs(x, y: integer);
begin
    if x < 1 then
        x := 1;
    if y < 1 then
        y := 1;
    write(#27, '[', y, ';', x, 'H');
end;

procedure crt_ensure_default_window;
begin
    if (crt_win_x1 = 1) and (crt_win_y1 = 1) and (crt_win_x2 = 80) and (crt_win_y2 = 25) then
    begin
        crt_win_x2 := kgpc_crt_screen_width();
        crt_win_y2 := kgpc_crt_screen_height();
    end;
end;

procedure clrscr;
var
    row: integer;
    col_count: integer;
    i: integer;
begin
    crt_ensure_default_window;
    col_count := crt_win_x2 - crt_win_x1 + 1;
    if col_count < 0 then
        col_count := 0;

    if (crt_win_x1 = 1) and (crt_win_y1 = 1) and
        (crt_win_x2 = kgpc_crt_screen_width()) and (crt_win_y2 = kgpc_crt_screen_height()) then
    begin
        // Match FPC's exact clrscr sequence: [6n[H[m[H[2J
        write(#27, '[6n');  // Query cursor position
        write(#27, '[H');   // Cursor home
        write(#27, '[m');   // Reset attributes
        write(#27, '[H');   // Cursor home again
        write(#27, '[2J');  // Clear screen
    end
    else
    begin
        for row := crt_win_y1 to crt_win_y2 do
        begin
            crt_gotoxy_abs(crt_win_x1, row);
            for i := 1 to col_count do
                write(' ');
        end;
        crt_gotoxy_abs(crt_win_x1, crt_win_y1);
    end;

    crt_cur_x := 1;
    crt_cur_y := 1;
<<<<<<< Updated upstream
=======
    // Removed gotoxy(1, 1) to match FPC's exact behavior
>>>>>>> Stashed changes
end;

procedure clreol;
var
    remaining: integer;
    i: integer;
    abs_x: integer;
    abs_y: integer;
begin
    crt_ensure_default_window;
    remaining := (crt_win_x2 - (crt_win_x1 + crt_cur_x - 1) + 1);
    if remaining < 0 then
        remaining := 0;
    abs_x := crt_win_x1 + crt_cur_x - 1;
    abs_y := crt_win_y1 + crt_cur_y - 1;
    crt_gotoxy_abs(abs_x, abs_y);
    for i := 1 to remaining do
        write(' ');
    crt_gotoxy_abs(abs_x, abs_y);
end;

procedure gotoxy(x, y: integer);
begin
    crt_ensure_default_window;
    if x < 1 then
        x := 1;
    if y < 1 then
        y := 1;
    crt_cur_x := x;
    crt_cur_y := y;
    crt_gotoxy_abs((crt_win_x1 + x - 1), (crt_win_y1 + y - 1));
end;

procedure window(x1, y1, x2, y2: integer);
begin
    if x1 < 1 then
        x1 := 1;
    if y1 < 1 then
        y1 := 1;
    if x2 < x1 then
        x2 := x1;
    if y2 < y1 then
        y2 := y1;

    crt_win_x1 := x1;
    crt_win_y1 := y1;
    crt_win_x2 := x2;
    crt_win_y2 := y2;

    crt_cur_x := 1;
    crt_cur_y := 1;
    gotoxy(1, 1);
end;

procedure textcolor(color: integer);
begin
    if color < 0 then
        color := 0;
    color := color mod 16;
    
    // Match FPC's exact behavior for text colors:
    // - Colors 0-7: [0;3Xm (normal colors)
    // - Colors 8-15: [0;9Xm (bright colors)
    if color < 8 then
        write(#27, '[0;', 30 + color, 'm')
    else
        write(#27, '[0;', 90 + (color - 8), 'm');
end;

procedure textbackground(color: integer);
begin
    if color < 0 then
        color := 0;
    color := color mod 16;
    
    // Match FPC's exact behavior:
    // - Black (0) becomes simple reset [m]
    // - Blue (1) uses [0;44m] (no bold)
    // - Red (4) uses [0;41m] (no bold)
    // - Other colors use [0;XXm] pattern (no bold)
    if color = 0 then
        write(#27, '[m')
    else if color = 1 then  // Blue
        write(#27, '[0;44m')
    else if color = 4 then  // Red
        write(#27, '[0;41m')
    else if color < 8 then
        write(#27, '[0;', 40 + color, 'm')
    else
        write(#27, '[0;', 100 + (color - 8), 'm');
end;

procedure highvideo;
begin
    write(#27, '[1m');
end;

procedure lowvideo;
begin
    write(#27, '[22m');
end;

procedure normvideo;
begin
    write(#27, '[m');  // Match FPC's exact reset pattern
end;

function readkey: char;
var
    v: integer;
begin
    v := kgpc_keyboard_read_char();
    if v < 0 then
        v := 0;
    readkey := chr(v and 255);
end;

function screenwidth: integer;
begin
    screenwidth := kgpc_crt_screen_width();
end;

function screenheight: integer;
begin
    screenheight := kgpc_crt_screen_height();
end;

end.
