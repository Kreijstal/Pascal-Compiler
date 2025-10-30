unit gmp;

interface

type
    NativeUInt = longint;
    AnsiString = string;

    MPInteger = record
        __gpc_mp_handle: Pointer;
    end;

procedure z_init(var value: MPInteger);
procedure z_clear(var value: MPInteger);
procedure z_set_ui(var value: MPInteger; operand: NativeUInt);
procedure z_add_ui(var value: MPInteger; operand: NativeUInt);
procedure z_ui_pow_ui(var value: MPInteger; base, exponent: NativeUInt);
function z_size(var value: MPInteger): NativeUInt;
procedure z_set(var dest: MPInteger; var src: MPInteger);
function z_add(var target: MPInteger; var operand: MPInteger): MPInteger;
function z_get_str(base: NativeUInt; var value: MPInteger): AnsiString;

implementation

procedure z_init(var value: MPInteger);
begin
    asm
        call gpc_gmp_z_init
    end;
end;

procedure z_clear(var value: MPInteger);
begin
    asm
        call gpc_gmp_z_clear
    end;
end;

procedure z_set_ui(var value: MPInteger; operand: NativeUInt);
begin
    asm
        call gpc_gmp_z_set_ui
    end;
end;

procedure z_add_ui(var value: MPInteger; operand: NativeUInt);
begin
    asm
        call gpc_gmp_z_add_ui
    end;
end;

procedure z_ui_pow_ui(var value: MPInteger; base, exponent: NativeUInt);
begin
    asm
        call gpc_gmp_z_ui_pow_ui
    end;
end;

function z_size(var value: MPInteger): NativeUInt;
var
    result_value: NativeUInt;
begin
    asm
        call gpc_gmp_z_size
        movq %rax, -8(%rbp)
    end;
    z_size := result_value;
end;

procedure z_set(var dest: MPInteger; var src: MPInteger);
begin
    asm
        call gpc_gmp_z_copy
    end;
end;

function z_add(var target: MPInteger; var operand: MPInteger): MPInteger;
begin
    asm
        call gpc_gmp_z_add
    end;
    z_add := target;
end;

function z_get_str(base: NativeUInt; var value: MPInteger): AnsiString;
var
    result_ptr: AnsiString;
begin
    asm
        call gpc_gmp_z_get_str
        movq %rax, -8(%rbp)
    end;
    z_get_str := result_ptr;
end;

end.
